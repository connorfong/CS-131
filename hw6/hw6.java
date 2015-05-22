/*I used the following websites:
	To learn how to concatenate arrays in Java:
	http://stackoverflow.com/questions/80476/how-to-concatenate-two-arrays-in-java
	To learn how to split arrays using copyOfRange:
	http://docs.oracle.com/javase/7/docs/api/java/util/Arrays.html
*/
	
import java.io.*;
import java.util.*;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;
import java.util.Arrays;

// a marker for code that you need to implement
class ImplementMe extends RuntimeException {}

// an RGB triple
class RGB {
    public int R, G, B;

    RGB(int r, int g, int b) {
	R = r;
	G = g;
	B = b;
    }

    public String toString() { return "(" + R + "," + G + "," + B + ")"; }

}

// code for creating a Gaussian filter
class Gaussian {

    protected static double gaussian(int x, int mu, double sigma) {
	return Math.exp( -(Math.pow((x-mu)/sigma,2.0))/2.0 );
    }

    public static double[][] gaussianFilter(int radius, double sigma) {
	int length = 2 * radius + 1;
	double[] hkernel = new double[length];
	for(int i=0; i < length; i++)
	    hkernel[i] = gaussian(i, radius, sigma);
	double[][] kernel2d = new double[length][length];
	double kernelsum = 0.0;
	for(int i=0; i < length; i++) {
	    for(int j=0; j < length; j++) {
		double elem = hkernel[i] * hkernel[j];
		kernelsum += elem;
		kernel2d[i][j] = elem;
	    }
	}
	for(int i=0; i < length; i++) {
	    for(int j=0; j < length; j++)
		kernel2d[i][j] /= kernelsum;
	}
	return kernel2d;
    }
}

// an object representing a single PPM image
class PPMImage {
    protected int width, height, maxColorVal;
    protected RGB[] pixels;

    PPMImage(int w, int h, int m, RGB[] p) {
	width = w;
	height = h;
	maxColorVal = m;
	pixels = p;
    }

	// parse a PPM file to produce a PPMImage
    public static PPMImage fromFile(String fname) throws FileNotFoundException, IOException {
	FileInputStream is = new FileInputStream(fname);
	BufferedReader br = new BufferedReader(new InputStreamReader(is));
	br.readLine(); // read the P6
	String[] dims = br.readLine().split(" "); // read width and height
	int width = Integer.parseInt(dims[0]);
	int height = Integer.parseInt(dims[1]);
	int max = Integer.parseInt(br.readLine()); // read max color value
	br.close();

	is = new FileInputStream(fname);
	    // skip the first three lines
	int newlines = 0;
	while (newlines < 3) {
	    int b = is.read();
	    if (b == 10)
		newlines++;
	}

	int MASK = 0xff;
	int numpixels = width * height;
	byte[] bytes = new byte[numpixels * 3];
        is.read(bytes);
	RGB[] pixels = new RGB[numpixels];
	for (int i = 0; i < numpixels; i++) {
	    int offset = i * 3;
	    pixels[i] = new RGB(bytes[offset] & MASK, bytes[offset+1] & MASK, bytes[offset+2] & MASK);
	}

	return new PPMImage(width, height, max, pixels);
    }

	// write a PPMImage object to a file
    public void toFile(String fname) throws IOException {
	FileOutputStream os = new FileOutputStream(fname);

	String header = "P6\n" + width + " " + height + "\n" + maxColorVal + "\n";
	os.write(header.getBytes());

	int numpixels = width * height;
	byte[] bytes = new byte[numpixels * 3];
	int i = 0;
	for (RGB rgb : pixels) {
	    bytes[i] = (byte) rgb.R;
	    bytes[i+1] = (byte) rgb.G;
	    bytes[i+2] = (byte) rgb.B;
	    i += 3;
	}
	os.write(bytes);
    }

    public PPMImage negate() {
		negImage r = new negImage(maxColorVal, pixels);
		ForkJoinPool pool = new ForkJoinPool();
		negImage result = pool.invoke(r);
		return new PPMImage(width, height, maxColorVal, result.pix());
    }

    public PPMImage mirrorImage() {
		mirImage r = new mirImage(width, height, maxColorVal, pixels);
		ForkJoinPool pool = new ForkJoinPool();
		mirImage result = pool.invoke(r);
		return new PPMImage(width, height, maxColorVal, result.pix());
    }

    public PPMImage gaussianBlur(int radius, double sigma) {
		double[][] gauss = Gaussian.gaussianFilter(radius, sigma);
		RGB[][] pix = new RGB[height][width];
		int curr = 0;
		for (int i = 0; i < height; i++) {
			for (int j = 0; j < width; j++) {
				pix[i][j] = pixels[curr];
				curr++;
			}
		}
		gaussImage r = new gaussImage(width, height, maxColorVal, pix, 0, height, height, radius, gauss);
		ForkJoinPool pool = new ForkJoinPool();
		gaussImage result = pool.invoke(r);
		return new PPMImage(width, height, maxColorVal, result.pix());
    }
}

class negImage extends RecursiveTask<negImage> {
	protected static final int SEQUENTIAL_THRESHOLD = 10000;
	protected int maxColorVal;
    protected RGB[] pixels;

    negImage(int m, RGB[] p) {
		maxColorVal = m;
		pixels = Arrays.copyOfRange(p, 0, p.length);
    }
	
	public RGB[] pix() { return pixels; }
	
	public negImage compute() {
		if (pixels.length < SEQUENTIAL_THRESHOLD) {
			for (int i = 0; i < pixels.length; i++) {
				int r = (maxColorVal - pixels[i].R);
				int g = (maxColorVal - pixels[i].G);
				int b = (maxColorVal - pixels[i].B);
				pixels[i] = new RGB(r, g, b);
			}
			return this;
		}
		else {
			int mid = (pixels.length/2);
			RGB[] pixels2 = Arrays.copyOfRange(pixels, mid, pixels.length);
			RGB[] pixels3 = Arrays.copyOfRange(pixels, 0, mid);
			negImage top = new negImage(maxColorVal, pixels3);
			negImage bot = new negImage(maxColorVal, pixels2);
			top.fork();
			negImage negbot = bot.compute();
			negImage negtop = top.join();
			System.arraycopy(negtop.pixels, 0, pixels, 0, negtop.pixels.length);
			System.arraycopy(negbot.pixels, 0, pixels, negtop.pixels.length, negbot.pixels.length);
			return this;
		}
	}
}

class mirImage extends RecursiveTask<mirImage> {
	protected static final int SEQUENTIAL_THRESHOLD = 10000;
	protected int width, height, maxColorVal;
    protected RGB[] pixels;

    mirImage(int w, int h, int m, RGB[] p) {
		width = w;
		height = h;
		maxColorVal = m;
		pixels = p;
    }
	
	public RGB[] pix() { return pixels; }
	
	public mirImage compute() {
		if (pixels.length < SEQUENTIAL_THRESHOLD) {
			for (int i = 0; i < height; i++) {
				RGB[] pixels2 = Arrays.copyOfRange(pixels, i*width, (i+1)*width);
				
				int left = 0;
				int right = pixels2.length-1;
				while (left < right) {
					RGB temp = pixels2[left];
					pixels2[left] = pixels2[right];
					pixels2[right] = temp;
					left++;
					right--;
				}
				System.arraycopy(pixels2, 0, pixels, i*width, pixels2.length);
			}
			return this;
		}
		else {
			int mid = (height/2);
			RGB[] pixels2 = Arrays.copyOfRange(pixels, 0, mid*width);
			RGB[] pixels3 = Arrays.copyOfRange(pixels, mid*width, pixels.length);
			mirImage top = new mirImage(width, mid, maxColorVal, pixels2);
			mirImage bot = new mirImage(width, (height-mid), maxColorVal, pixels3);
			top.fork();
			mirImage mirbot = bot.compute();
			mirImage mirtop = top.join();
			System.arraycopy(mirtop.pixels, 0, pixels, 0, mirtop.pixels.length);
			System.arraycopy(mirbot.pixels, 0, pixels, mirtop.pixels.length, mirbot.pixels.length);
			return this;
		}
	}
}

class gaussImage extends RecursiveTask<gaussImage> {
	protected static final int SEQUENTIAL_THRESHOLD = 10000;
	protected int width, height, maxColorVal;
    protected RGB[][] pixels;
	protected int radius, startHeight, endHeight, totalHeight;
	protected double[][] filter;
	protected RGB[] result;
	
	gaussImage(int w, int h, int m, RGB[][] p, int sh, int eh, int th, int r, double[][] gauss) {
		width = w;
		height = h;
		maxColorVal = m;
		pixels = p;
		startHeight = sh;
		endHeight = eh;
		totalHeight = th;
		radius = r;
		filter = gauss;
		result = new RGB[(width*height)];
    }
	
	public RGB[] pix() { return result; }
	
	public gaussImage compute() {
		if ((width*height) < SEQUENTIAL_THRESHOLD) {
			int curr = 0;
			for (int i = startHeight; i < endHeight; i++) {
				for (int j = 0; j < width; j++) {
					int xpos = j-radius;
					int xend = j+radius+1;
					int ypos = i-radius;
					int yend = i+radius+1;
					int yfilterpos = 0;
					double sumR = 0;
					double sumG = 0;
					double sumB = 0;
					while (ypos != yend) {
						xpos = j-radius;
						int xfilterpos = 0;
						while (xpos != xend) {
							int xtemp = xpos;
							int ytemp = ypos;
							if (xtemp < 0) 
								xtemp = 0; 
							if (ytemp < 0) 
								ytemp = 0; 
							if (xtemp > width-1) 
								xtemp = width-1; 
							if (ytemp > totalHeight-1) 
								ytemp = totalHeight-1; 
							double r = (pixels[ytemp][xtemp].R)*(filter[yfilterpos][xfilterpos]);
							double g = (pixels[ytemp][xtemp].G)*(filter[yfilterpos][xfilterpos]);
							double b = (pixels[ytemp][xtemp].B)*(filter[yfilterpos][xfilterpos]);
							sumR = sumR + r;
							sumG = sumG + g;
							sumB = sumB + b;
							xpos = xpos+1;
							xfilterpos = xfilterpos+1;
						}
						ypos = ypos+1;
						yfilterpos = yfilterpos+1;
					}
					double R = Math.round(sumR);
					double G = Math.round(sumG);
					double B = Math.round(sumB);
					result[curr] = new RGB((int)R, (int)G, (int)B);
					curr = curr+1;
				}
			}
			return this;
		}	
		else {
			int mid = (startHeight+endHeight)/2;
			int nHeight = height/2;
			gaussImage top = new gaussImage(width, nHeight, maxColorVal, pixels, startHeight, mid, totalHeight, radius, filter);
			gaussImage bot = new gaussImage(width, (height-nHeight), maxColorVal, pixels, mid, endHeight, totalHeight, radius, filter);
			top.fork();
			gaussImage gaussbot = bot.compute();
			gaussImage gausstop = top.join();
			System.arraycopy(gausstop.result, 0, result, 0, gausstop.result.length);
			System.arraycopy(gaussbot.result, 0, result, gausstop.result.length, gaussbot.result.length);
			return this;
		}
	}
}

class Main {

    public static void main(String[] args) {
		RGB[] temp = new RGB[1];
		temp[0] = new RGB(0,0,0);
		PPMImage x = new PPMImage(0,0,0,temp);
		try { x = x.fromFile("florence.ppm"); }
		catch(FileNotFoundException e) {return;}
		catch(IOException e) {return;}
		PPMImage y = x.gaussianBlur(5, 5);
		try { y.toFile("florencegauss.ppm"); }
		catch(IOException e) { return; }
    }
}

class PPMImageTest {
	public static void main(String[] args) {
		// Setup the timer
		final long NANOTOMETRIC = 1000000000;
		long startTime, endTime;
		double elapsedTime;

		// Parse image
		PPMImage florence = new PPMImage(0, 0, 0, null);
		System.out.println("Parsing florence.ppm...");
		try {
			florence = PPMImage.fromFile("florence.ppm");
		} catch (FileNotFoundException e) {
			System.out.println("File not found!");
		} catch (IOException e) {
			System.out.println("I/O Error!");
		}

		// Negate
		System.out.print("Negating image...");
		startTime = System.nanoTime();
		PPMImage negate = florence.negate();
		endTime = System.nanoTime();
		elapsedTime = (double) (endTime-startTime)/NANOTOMETRIC;
		System.out.print("takes " + elapsedTime + " seconds\n");
		System.out.println("Generating florence_negate.ppm...");
		try {
			negate.toFile("florence_negate.ppm");
		} catch (IOException e) {
			System.out.println("I/O Error!");
		}

		// Mirror
		System.out.print("Mirroring image...");
		startTime = System.nanoTime();
		PPMImage mirror = florence.mirrorImage();
		endTime = System.nanoTime();
		elapsedTime = (double) (endTime-startTime)/NANOTOMETRIC;
		System.out.print("takes " + elapsedTime + " seconds\n");
		System.out.println("Generating florence_mirror.ppm...");
		try {
			mirror.toFile("florence_mirror.ppm");
		} catch (IOException e) {
			System.out.println("I/O Error!");
		}

		// Gaussian blur
		System.out.print("Blurring (r=3, s=2) image...");
		startTime = System.nanoTime();
		PPMImage gaussian = florence.gaussianBlur(3, 2);
		endTime = System.nanoTime();
		elapsedTime = (double) (endTime-startTime)/NANOTOMETRIC;
		System.out.print("takes " + elapsedTime + " seconds\n");
		System.out.println("Generating florence_gaussian.ppm...");
		try {
			gaussian.toFile("florence_gaussian.ppm");
		} catch (IOException e) {
			System.out.println("I/O Error!");
		}
		// Gaussian blur with greater radius (should be fast)
		System.out.print("Blurring (r=50, s=25) image...");
		startTime = System.nanoTime();
		PPMImage gaussian2 = florence.gaussianBlur(50, 25);
		endTime = System.nanoTime();
		elapsedTime = (double) (endTime-startTime)/NANOTOMETRIC;
		System.out.print("takes " + elapsedTime + " seconds\n");
		System.out.println("Generating florence_gaussian2.ppm...");
		try {
			gaussian2.toFile("florence_gaussian2.ppm");
		} catch (IOException e) {
			System.out.println("I/O Error!");
		}
	}
}







