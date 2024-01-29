package test.calc.generated;

import java.text.ParseException;
import java.util.ArrayList;

public class Parser {
    private final Lexer lexer = new Lexer();
    public Prog parse(final String input) throws ParseException {
        lexer.set(input);
        return prog();
    }
    private double fac(double n) {
	     double f = 1;
	     for (int i = 2; i <= n; ++i) f *= i;
	     return f;
	}
	private double fac2(double n) {
	    double f = 1;
	    for (int i = ((n % 2 == 0) ? 2 : 1); i <= n; i += 2) f *= i;
	    return f;
	}
    private abstract static class Nt {
        private final String name;
        private final ArrayList<Nt> children = new ArrayList<>();
        public Nt(String name) { this.name = name; }
        public void addChild(Nt nt) { children.add(nt); }
        public String name() { return name; }
        public ArrayList<Nt> children() { return children; }
    }
    private static class Term extends Nt {
        public String value;
        public Term(String name, String value) { super(name); this.value = value; }
    }
	public static class Prog extends Nt {
		public Prog() { super("prog"); }
		double val; 
	}
	private Prog prog() throws ParseException {
		Prog res = new Prog();
		switch (lexer.token()) {
			case NUMBER, MIN, MAX, LR, LS, LC, SIN, MINUS -> {
				C v0 = c(); res.addChild(v0);
				res.val = v0.val;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class C extends Nt {
		public C() { super("c"); }
		double val; 
	}
	private C c() throws ParseException {
		C res = new C();
		switch (lexer.token()) {
			case NUMBER, MIN, MAX, LR, LS, LC, SIN, MINUS -> {
				E v0 = e(); res.addChild(v0);
				Ci v1 = ci(v0.val); res.addChild(v1);
				res.val = v1.val;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class Ci extends Nt {
		public Ci() { super("ci"); }
		double val; 
	}
	private Ci ci(double acc) throws ParseException {
		Ci res = new Ci();
		switch (lexer.token()) {
			case RR, COMMA, RS, RC, EOF -> {
				Term v0 = new Term("EPS", lexer.value()); res.addChild(v0);
				res.val = acc;
			}
			case EXL2 -> {
				Term v0 = new Term("EXL2", lexer.value()); res.addChild(v0); lexer.nextToken();
				Ci v1 = ci(fac2(acc)); res.addChild(v1);
				res.val = v1.val;
			}
			case EXL -> {
				Term v0 = new Term("EXL", lexer.value()); res.addChild(v0); lexer.nextToken();
				Ci v1 = ci(fac(acc)); res.addChild(v1);
				res.val = v1.val;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class E extends Nt {
		public E() { super("e"); }
		double val; 
	}
	private E e() throws ParseException {
		E res = new E();
		switch (lexer.token()) {
			case NUMBER, MIN, MAX, LR, LS, LC, SIN, MINUS -> {
				T v0 = t(); res.addChild(v0);
				Ei v1 = ei(v0.val); res.addChild(v1);
				res.val = v1.val;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class Ei extends Nt {
		public Ei() { super("ei"); }
		double val; 
	}
	private Ei ei(double acc) throws ParseException {
		Ei res = new Ei();
		switch (lexer.token()) {
			case RR, COMMA, RS, RC, EXL, EXL2, EOF -> {
				Term v0 = new Term("EPS", lexer.value()); res.addChild(v0);
				res.val = acc;
			}
			case PLUS -> {
				Term v0 = new Term("PLUS", lexer.value()); res.addChild(v0); lexer.nextToken();
				T v1 = t(); res.addChild(v1);
				Ei v2 = ei(acc+v1.val); res.addChild(v2);
				res.val = v2.val;
			}
			case MINUS -> {
				Term v0 = new Term("MINUS", lexer.value()); res.addChild(v0); lexer.nextToken();
				T v1 = t(); res.addChild(v1);
				Ei v2 = ei(acc-v1.val); res.addChild(v2);
				res.val = v2.val;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class T extends Nt {
		public T() { super("t"); }
		double val; 
	}
	private T t() throws ParseException {
		T res = new T();
		switch (lexer.token()) {
			case NUMBER, MIN, MAX, LR, LS, LC, SIN, MINUS -> {
				F v0 = f(); res.addChild(v0);
				Ti v1 = ti(v0.val); res.addChild(v1);
				res.val = v1.val;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class Ti extends Nt {
		public Ti() { super("ti"); }
		double val; 
	}
	private Ti ti(double acc) throws ParseException {
		Ti res = new Ti();
		switch (lexer.token()) {
			case RR, COMMA, RS, RC, EXL, EXL2, EOF, PLUS, MINUS -> {
				Term v0 = new Term("EPS", lexer.value()); res.addChild(v0);
				res.val = acc;
			}
			case MUL -> {
				Term v0 = new Term("MUL", lexer.value()); res.addChild(v0); lexer.nextToken();
				F v1 = f(); res.addChild(v1);
				Ti v2 = ti(acc*v1.val); res.addChild(v2);
				res.val = v2.val;
			}
			case DIV -> {
				Term v0 = new Term("DIV", lexer.value()); res.addChild(v0); lexer.nextToken();
				F v1 = f(); res.addChild(v1);
				Ti v2 = ti(acc/v1.val); res.addChild(v2);
				res.val = v2.val;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class F extends Nt {
		public F() { super("f"); }
		double val; 
	}
	private F f() throws ParseException {
		F res = new F();
		switch (lexer.token()) {
			case NUMBER, MIN, MAX, LR, LS, LC, SIN, MINUS -> {
				P v0 = p(); res.addChild(v0);
				Pi v1 = pi(v0.val); res.addChild(v1);
				res.val = v1.val;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class P extends Nt {
		public P() { super("p"); }
		double val; 
	}
	private P p() throws ParseException {
		P res = new P();
		switch (lexer.token()) {
			case NUMBER -> {
				N v0 = n(); res.addChild(v0);
				res.val = v0.val;
			}
			case MINUS -> {
				Term v0 = new Term("MINUS", lexer.value()); res.addChild(v0); lexer.nextToken();
				P v1 = p(); res.addChild(v1);
				res.val = -v1.val;
			}
			case LR -> {
				Term v0 = new Term("LR", lexer.value()); res.addChild(v0); lexer.nextToken();
				C v1 = c(); res.addChild(v1);
				Term v2 = new Term("RR", lexer.value()); res.addChild(v2); lexer.nextToken();
				res.val = v1.val;
			}
			case SIN -> {
				Term v0 = new Term("SIN", lexer.value()); res.addChild(v0); lexer.nextToken();
				Term v1 = new Term("LR", lexer.value()); res.addChild(v1); lexer.nextToken();
				C v2 = c(); res.addChild(v2);
				Term v3 = new Term("RR", lexer.value()); res.addChild(v3); lexer.nextToken();
				res.val = Math.sin(v2.val);
			}
			case MAX -> {
				Term v0 = new Term("MAX", lexer.value()); res.addChild(v0); lexer.nextToken();
				Term v1 = new Term("LR", lexer.value()); res.addChild(v1); lexer.nextToken();
				C v2 = c(); res.addChild(v2);
				Term v3 = new Term("COMMA", lexer.value()); res.addChild(v3); lexer.nextToken();
				C v4 = c(); res.addChild(v4);
				Term v5 = new Term("RR", lexer.value()); res.addChild(v5); lexer.nextToken();
				res.val = Math.max(v2.val, v4.val);
			}
			case MIN -> {
				Term v0 = new Term("MIN", lexer.value()); res.addChild(v0); lexer.nextToken();
				Term v1 = new Term("LR", lexer.value()); res.addChild(v1); lexer.nextToken();
				C v2 = c(); res.addChild(v2);
				Term v3 = new Term("COMMA", lexer.value()); res.addChild(v3); lexer.nextToken();
				C v4 = c(); res.addChild(v4);
				Term v5 = new Term("RR", lexer.value()); res.addChild(v5); lexer.nextToken();
				res.val = Math.min(v2.val, v4.val);
			}
			case LS -> {
				Term v0 = new Term("LS", lexer.value()); res.addChild(v0); lexer.nextToken();
				C v1 = c(); res.addChild(v1);
				Term v2 = new Term("RS", lexer.value()); res.addChild(v2); lexer.nextToken();
				res.val = (int) v1.val;
			}
			case LC -> {
				Term v0 = new Term("LC", lexer.value()); res.addChild(v0); lexer.nextToken();
				C v1 = c(); res.addChild(v1);
				Term v2 = new Term("RC", lexer.value()); res.addChild(v2); lexer.nextToken();
				res.val = v1.val % 1;
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class Pi extends Nt {
		public Pi() { super("pi"); }
		double val; 
	}
	private Pi pi(double acc) throws ParseException {
		Pi res = new Pi();
		switch (lexer.token()) {
			case DIV, RR, COMMA, RS, RC, MUL, EXL, EXL2, EOF, PLUS, MINUS -> {
				Term v0 = new Term("EPS", lexer.value()); res.addChild(v0);
				res.val = acc;
			}
			case POW -> {
				Term v0 = new Term("POW", lexer.value()); res.addChild(v0); lexer.nextToken();
				P v1 = p(); res.addChild(v1);
				Pi v2 = pi(v1.val); res.addChild(v2);
				res.val = Math.pow(acc, v2.val);
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
	public static class N extends Nt {
		public N() { super("n"); }
		double val; 
	}
	private N n() throws ParseException {
		N res = new N();
		switch (lexer.token()) {
			case NUMBER -> {
				Term v0 = new Term("NUMBER", lexer.value()); res.addChild(v0); lexer.nextToken();
				res.val = Double.parseDouble(v0.value);
			}
			default -> throw new ParseException("no such rule", lexer.pos());
		}
		return res;
	}
}