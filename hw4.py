import math
import struct

# Copy the dictionary given so that the original dictionary will be unchanged
# The "pixels" element is copied by creating a new list of pixels
def copyDict(ppm):
	d = {}
	d["width"] = ppm["width"]
	d["height"] = ppm["height"]
	d["max"] = ppm["max"]
	d["pixels"] = list(ppm["pixels"])
	return d

# PROBLEM 1

# parse the file named fname into a dictionary of the form 
# {'width': int, 'height' : int, 'max' : int, pixels : (int * int * int) list}
def parsePPM(fname):
	f = open(fname, "rb")
	l = f.readline()
	l = f.readline()
	wh = l.split()
	width = int(wh[0])
	height = int(wh[1])
	l = f.readline()
	max = int(l)
	byt = f.read(3)
	pixels = []
	while byt != "": 
		pix = struct.unpack("3B", byt)
		pixels.append(pix)
		byt = f.read(3)
	d = {"width" : width, "height" : height, "max" : max, "pixels" : pixels}
	f.close()
	return d
		

# write the given ppm dictionary as a PPM image file named fname
# the function should not return anything
def unparsePPM(ppm, fname):
	f = open(fname, "wb")
	f.write('P6'+'\n')
	f.write(str(ppm["width"])+' '+str(ppm["height"])+'\n')
	f.write(str(ppm["max"])+'\n')
	for (R, G, B) in ppm["pixels"]:
		f.write(struct.pack("3B", R, G, B))
	f.close()
	return
   


# PROBLEM 2
def negate(ppm):
	d = copyDict(ppm)
	f = (lambda (R, G, B): (ppm["max"]-R, ppm["max"]-G, ppm["max"]-B))
	d["pixels"] = map(f, d["pixels"])
	return d
		
		



# PROBLEM 3
def mirrorImage(ppm):
	d = copyDict(ppm)
	currh = 0
	temp = ppm["pixels"]
	pix = []
	while currh != ppm["height"]:
		l = temp[(ppm["width"]*currh):(ppm["width"]*(currh+1))]
		l.reverse()
		pix = pix + l
		currh = currh+1
	d["pixels"] = pix
	return d

# PROBLEM 4

# produce a greyscale version of the given ppm dictionary.
# the resulting dictionary should have the same format, 
# except it will only have a single value for each pixel, 
# rather than an RGB triple.
def greyscale(ppm):
	d = copyDict(ppm)
	pix = []
	for (R, G, B) in d["pixels"]:
		val = .299 * R + .587 * G + .114 * B
		pix.append(round(val))
	d["pixels"] = pix
	return d

# take a dictionary produced by the greyscale function and write it as a PGM image file named fname
# the function should not return anything
def unparsePGM(pgm, fname):
	f = open(fname, "wb")
	f.write('P5'+'\n')
	f.write(str(pgm["width"])+' '+str(pgm["height"])+'\n')
	f.write(str(pgm["max"])+'\n')
	for i in pgm["pixels"]:
		f.write(struct.pack("B", i))
	f.close()
	return


# PROBLEM 5

# gaussian blur code adapted from:
# http://stackoverflow.com/questions/8204645/implementing-gaussian-blur-how-to-calculate-convolution-matrix-kernel
def gaussian(x, mu, sigma):
  return math.exp( -(((x-mu)/(sigma))**2)/2.0 )

def gaussianFilter(radius, sigma):
    # compute the actual kernel elements
    hkernel = [gaussian(x, radius, sigma) for x in range(2*radius+1)]
    vkernel = [x for x in hkernel]
    kernel2d = [[xh*xv for xh in hkernel] for xv in vkernel]

    # normalize the kernel elements
    kernelsum = sum([sum(row) for row in kernel2d])
    kernel2d = [[x/kernelsum for x in row] for row in kernel2d]
    return kernel2d

# blur a given ppm dictionary, returning a new dictionary  
# the blurring uses a gaussian filter produced by the above function
def gaussianBlur(ppm, radius, sigma):
    # obtain the filter
	gfilter = gaussianFilter(radius, sigma)
	d = copyDict(ppm)
	a = []
	j, count = 0, 0
	# Create a 2d array of pixels
	while j != d["height"]:
		i = 0
		rows = []
		while i != d["width"]:
			rows.append(d["pixels"][count])
			count = count+1
			i = i+1
		a.append(rows)
		j= j+1
	y, curr = 0, 0
	while y != d["height"]:
		x = 0
		# Calculate the area affected by the blur
		while x != d["width"]:
			xstart = x-radius
			xend = x+radius+1
			ystart = y-radius
			yend = y+radius+1
			# Check if blur is in bounds of picture
			if (xstart) < 0 or (xend) > (d["width"]) or (ystart) < 0 or (yend) > (d["height"]):
				x = x+1
				curr = curr+1
				continue
			else:
				#temp = a[(x-radius):(x+radius+1), (y-radius):(y+radius+1)]
				sumR, sumG, sumB = 0, 0, 0
				yfilterstart = 0
				# Iterate over the blurred area of the 2d array, multiply by filter
				while ystart != yend:
					xstart = x-radius
					xfilterstart = 0
					while xstart != xend:
						R = a[ystart][xstart][0]*gfilter[yfilterstart][xfilterstart]
						G = a[ystart][xstart][1]*gfilter[yfilterstart][xfilterstart]
						B = a[ystart][xstart][2]*gfilter[yfilterstart][xfilterstart]
						sumR = sumR + R
						sumG = sumG + G
						sumB = sumB + B
						xstart = xstart+1
						xfilterstart = xfilterstart+1
					ystart = ystart+1
					yfilterstart = yfilterstart+1
				d["pixels"][curr] = (round(sumR), round(sumG), round(sumB))
				curr = curr+1
				x = x+1
		y = y+1
	return d
		
	
