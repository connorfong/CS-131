
// import lists and other data structures from the Java standard library
import java.util.*;

// a type for arithmetic expressions
interface AExp {
     double eval(); 	                       // Problem 1a
     List<Sopn> toRPN(); 	               // Problem 1c
	 
	 List<Sopn> addToList(List<Sopn> l);
}

class Num implements AExp {
	protected double n;
	Num(double n) { this.n = n; }
	public double eval() { return n;}
	public List<Sopn> toRPN() {
		List<Sopn> instrs = new LinkedList<Sopn>();
		instrs.add(new Push(n));
		return instrs;
	}
	
	public List<Sopn> addToList(List<Sopn> l) {
		l.add(new Push(n));
		return l;
	}
	
	public String toString() {
		return "Num " + Double.toString(n);
	}
}

class BinOp implements AExp {
	protected AExp oper1;
	protected AExp oper2;
	protected Op op;
	
	BinOp(AExp oper1, Op op, AExp oper2) { 
		this.oper1 = oper1;
		this.oper2 = oper2;
		this.op = op;
	}
	
	public double eval() {
		return op.calculate(oper1.eval(), oper2.eval());
	}
	
	public List<Sopn> toRPN() {
		List<Sopn> instrs = new LinkedList<Sopn>();
		oper1.addToList(instrs);
		oper2.addToList(instrs);
		instrs.add(new Calculate(op));
		return instrs;
	}
	
	public List<Sopn> addToList(List<Sopn> l) {
		oper1.addToList(l);
		oper2.addToList(l);
		l.add(new Calculate(op));
		return l;
	}
}
	

// a representation of four arithmetic operators
enum Op {
    PLUS { public double calculate(double a1, double a2) { return a1 + a2; } },
    MINUS { public double calculate(double a1, double a2) { return a1 - a2; } },
    TIMES { public double calculate(double a1, double a2) { return a1 * a2; } },
    DIVIDE { public double calculate(double a1, double a2) { return a1 / a2; } };

    abstract double calculate(double a1, double a2);
}

// a type for stack operations
interface Sopn {
	Stack<Double> calc(Stack<Double> st);
	String toString();
}

class Push implements Sopn {
	protected double n;
	Push(double x) { n = x; }
	
	public Stack<Double> calc(Stack<Double> st) {
		st.push(n);
		return st;
	}
	
	public String toString() {
		return "Push " + Double.toString(n);
	}
}

class Swap implements Sopn {
	public Stack<Double> calc(Stack<Double> st) {
		double x = st.pop();
		double y = st.pop();
		st.push(x);
		st.push(y);
		return st;
	}
	
	public String toString() {
		return "Swap";
	}
}

class Calculate implements Sopn {
	protected Op op;
	Calculate(Op op) { this.op = op; }
	
	public Stack<Double> calc(Stack<Double> st) {
		double oper2 = st.pop();
		double oper1 = st.pop();
		st.push(op.calculate(oper1, oper2));
		return st;
	}
	
	public String toString() {
		return "Calculate " + op.toString();
	}
}

// an RPN expression is essentially a wrapper around a list of stack operations
class RPNExp {
    protected List<Sopn> instrs;

    public RPNExp(List<Sopn> instrs) { this.instrs = instrs; }

    public double eval() {
		Stack<Double> st = new Stack<Double>();
		for (Sopn s : instrs)
			s.calc(st);
		return st.pop();
	} 
}


class CalcTest {
    public static void main(String[] args) {
	    // a test for Problem 1a
	 AExp aexp =
	     new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
	 	      Op.TIMES,
	 	      new Num(3.0));
			  
	 AExp aexp2 =
	     new BinOp(new BinOp(new Num(3.0), Op.MINUS, new Num(2.0)),
	 	      Op.TIMES,
	 	      new Num(3.0));
	 System.out.println("aexp evaluates to " + aexp.eval()); // aexp evaluates to 9.0
	 System.out.println("aexp2 evaluates to " + aexp2.eval());

	// a test for Problem 1b
	List<Sopn> instrs = new LinkedList<Sopn>();
	instrs.add(new Push(1.0));
	instrs.add(new Push(2.0));
	instrs.add(new Calculate(Op.PLUS));
	instrs.add(new Push(3.0));
	instrs.add(new Swap());
	instrs.add(new Calculate(Op.TIMES));
	RPNExp rpnexp = new RPNExp(instrs);
	
	List<Sopn> instrs2 = new LinkedList<Sopn>();
	instrs2.add(new Push(3.0));
	instrs2.add(new Push(2.0));
	instrs2.add(new Calculate(Op.MINUS));
	instrs2.add(new Push(3.0));
	instrs2.add(new Swap());
	instrs2.add(new Calculate(Op.DIVIDE));
	RPNExp rpnexp2 = new RPNExp(instrs2);
	System.out.println("rpnexp evaluates to " + rpnexp.eval());  // rpnexp evaluates to 9.0
	System.out.println("rpnexp2 evaluates to " + rpnexp2.eval());
	// a test for Problem 1c
	System.out.println("aexp converts to " + aexp.toRPN());

    }
}


interface Dict<K,V> {
    void put(K k, V v);
    V get(K k) throws NotFoundException;
}

class NotFoundException extends Exception {}


// Problem 2a
class DictImpl2<K,V> implements Dict<K,V> {
    protected Node<K,V> root;

    DictImpl2() { this.root = new Empty<K,V>(); }

    public void put(K k, V v) { this.root = root.put(k, v); }

    public V get(K k) throws NotFoundException { return root.get(k); }
}

interface Node<K,V> {
	Node<K, V> put(K k, V v);
	V get(K k) throws NotFoundException;
}

class Empty<K,V> implements Node<K,V> {
    Empty() {}
	
	public Node<K, V> put(K k, V v) {
		return new Entry<K, V>(k, v, new Empty<K, V>());
	}
	
	public V get(K k) throws NotFoundException { 
		throw new NotFoundException(); 
	}
}

class Entry<K,V> implements Node<K,V> {
    protected K k;
    protected V v;
    protected Node<K,V> next;

    Entry(K k, V v, Node<K,V> next) {
	this.k = k;
	this.v = v;
	this.next = next;
    }
	
	public Node<K, V> put(K k, V v) {
		if (k.equals(this.k)) {
			this.v = v;
			return this;
		}
		else {
			this.next = next.put(k, v);
			return this;
		}
	}
	
	public V get(K k) throws NotFoundException {
		if (k.equals(this.k))
			return v;
		else
			return next.get(k);
	}
}


interface DictFun<A,R> {
    R invoke(A a) throws NotFoundException;
}

// Problem 2b
class DictImpl3<K,V> implements Dict<K,V> {
    protected DictFun<K,V> dFun;

    DictImpl3() { 
		dFun = new DictFun<K,V>() {
			public V invoke(K k) throws NotFoundException {
				throw new NotFoundException();
			}
		};
	}

    public void put(K k, V v) { 
		final K key = k;
		final V val = v;
		final DictFun<K,V> prevDict = dFun;
		dFun = new DictFun<K,V>() {
			public V invoke(K kx) throws NotFoundException {
				if (kx.equals(key)) 
					return val;
				else 
					return prevDict.invoke(kx);
			}
		};
	}

    public V get(K k) throws NotFoundException { return dFun.invoke(k); }
}


class Pair<A,B> {
    protected A fst;
    protected B snd;

    Pair(A fst, B snd) { this.fst = fst; this.snd = snd; }

    A fst() { return fst; }
    B snd() { return snd; }
}

// Problem 2c
interface FancyDict<K,V> extends Dict<K,V> {
    void clear();
    boolean containsKey(K k);
    void putAll(List<Pair<K,V>> entries);
}

class FancyDictImpl2<K,V> extends DictImpl2<K,V> implements FancyDict<K,V> {
    FancyDictImpl2() { this.root = new Empty<K,V>(); }
	
	public void clear() { this.root = new Empty<K,V>(); }
	
	public boolean containsKey(K k) {
		try {
			root.get(k);
			return true;
		} catch(NotFoundException e) {
			return false;
		}
	}
	
	public void putAll(List<Pair<K,V>> entries) {
		for (Pair<K,V> p : entries)
			root.put(p.fst(), p.snd());
	}
}


class DictTest {
    public static void main(String[] args) {

	// a test for Problem 2a
	Dict<String,Integer> dict1 = new DictImpl2<String,Integer>();
	dict1.put("hello", 23);
	dict1.put("bye", 45);
	try {
	    System.out.println("bye maps to " + dict1.get("bye")); // prints 45
		System.out.println("hello maps to " + dict1.get("hello"));
	    System.out.println("hi maps to " + dict1.get("hi"));  // throws an exception
	} catch(NotFoundException e) {
	    System.out.println("not found!");  // prints "not found!"
	}

	// a test for Problem 2b
	Dict<String,Integer> dict2 = new DictImpl3<String,Integer>();
	dict2.put("hello", 23);
	dict2.put("bye", 45);
	try {
	    System.out.println("bye maps to " + dict2.get("bye"));  // prints 45
		System.out.println("hello maps to " + dict1.get("hello"));
		System.out.println("hi maps to " + dict2.get("hi"));   // throws an exception
	} catch(NotFoundException e) {
	    System.out.println("not found!");  // prints "not found!"
	}

	// a test for Problem 2c
	FancyDict<String,Integer> dict3 = new FancyDictImpl2<String,Integer>();
	dict3.put("hello", 23);
	dict3.put("bye", 45);
	System.out.println(dict3.containsKey("bye")); // prints true
	dict3.clear();
	System.out.println(dict3.containsKey("bye")); // prints false

    }
}
