// Generated from src/antlr/Input.g4 by ANTLR 4.13.1
package antlr;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue", "this-escape"})
public class InputLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, NT_NAME=3, T_NAME=4, ATTR=5, CODE=6, REGEX=7, ARR=8, COL=9, 
		OR=10, EXTRA_CODE=11, WS=12;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "NT_NAME", "T_NAME", "ATTR", "CODE", "REGEX", "ARR", 
			"COL", "OR", "EXTRA_CODE", "WS"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'eps'", "'='", null, null, null, null, null, "'->'", "':'", "'|'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, "NT_NAME", "T_NAME", "ATTR", "CODE", "REGEX", "ARR", 
			"COL", "OR", "EXTRA_CODE", "WS"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public InputLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Input.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\u0004\u0000\fb\u0006\uffff\uffff\u0002\u0000\u0007\u0000\u0002\u0001"+
		"\u0007\u0001\u0002\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004"+
		"\u0007\u0004\u0002\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007"+
		"\u0007\u0007\u0002\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b"+
		"\u0007\u000b\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0001"+
		"\u0001\u0001\u0001\u0002\u0004\u0002!\b\u0002\u000b\u0002\f\u0002\"\u0001"+
		"\u0003\u0001\u0003\u0005\u0003\'\b\u0003\n\u0003\f\u0003*\t\u0003\u0001"+
		"\u0004\u0001\u0004\u0005\u0004.\b\u0004\n\u0004\f\u00041\t\u0004\u0001"+
		"\u0004\u0001\u0004\u0001\u0005\u0001\u0005\u0005\u00057\b\u0005\n\u0005"+
		"\f\u0005:\t\u0005\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006\u0005"+
		"\u0006@\b\u0006\n\u0006\f\u0006C\t\u0006\u0001\u0006\u0001\u0006\u0001"+
		"\u0007\u0001\u0007\u0001\u0007\u0001\b\u0001\b\u0001\t\u0001\t\u0001\n"+
		"\u0001\n\u0001\n\u0001\n\u0001\n\u0005\nS\b\n\n\n\f\nV\t\n\u0001\n\u0001"+
		"\n\u0001\n\u0001\n\u0001\u000b\u0004\u000b]\b\u000b\u000b\u000b\f\u000b"+
		"^\u0001\u000b\u0001\u000b\u0004/8AT\u0000\f\u0001\u0001\u0003\u0002\u0005"+
		"\u0003\u0007\u0004\t\u0005\u000b\u0006\r\u0007\u000f\b\u0011\t\u0013\n"+
		"\u0015\u000b\u0017\f\u0001\u0000\u0004\u0001\u0000az\u0001\u0000AZ\u0002"+
		"\u000009AZ\u0003\u0000\t\n\r\r  h\u0000\u0001\u0001\u0000\u0000\u0000"+
		"\u0000\u0003\u0001\u0000\u0000\u0000\u0000\u0005\u0001\u0000\u0000\u0000"+
		"\u0000\u0007\u0001\u0000\u0000\u0000\u0000\t\u0001\u0000\u0000\u0000\u0000"+
		"\u000b\u0001\u0000\u0000\u0000\u0000\r\u0001\u0000\u0000\u0000\u0000\u000f"+
		"\u0001\u0000\u0000\u0000\u0000\u0011\u0001\u0000\u0000\u0000\u0000\u0013"+
		"\u0001\u0000\u0000\u0000\u0000\u0015\u0001\u0000\u0000\u0000\u0000\u0017"+
		"\u0001\u0000\u0000\u0000\u0001\u0019\u0001\u0000\u0000\u0000\u0003\u001d"+
		"\u0001\u0000\u0000\u0000\u0005 \u0001\u0000\u0000\u0000\u0007$\u0001\u0000"+
		"\u0000\u0000\t+\u0001\u0000\u0000\u0000\u000b4\u0001\u0000\u0000\u0000"+
		"\r=\u0001\u0000\u0000\u0000\u000fF\u0001\u0000\u0000\u0000\u0011I\u0001"+
		"\u0000\u0000\u0000\u0013K\u0001\u0000\u0000\u0000\u0015M\u0001\u0000\u0000"+
		"\u0000\u0017\\\u0001\u0000\u0000\u0000\u0019\u001a\u0005e\u0000\u0000"+
		"\u001a\u001b\u0005p\u0000\u0000\u001b\u001c\u0005s\u0000\u0000\u001c\u0002"+
		"\u0001\u0000\u0000\u0000\u001d\u001e\u0005=\u0000\u0000\u001e\u0004\u0001"+
		"\u0000\u0000\u0000\u001f!\u0007\u0000\u0000\u0000 \u001f\u0001\u0000\u0000"+
		"\u0000!\"\u0001\u0000\u0000\u0000\" \u0001\u0000\u0000\u0000\"#\u0001"+
		"\u0000\u0000\u0000#\u0006\u0001\u0000\u0000\u0000$(\u0007\u0001\u0000"+
		"\u0000%\'\u0007\u0002\u0000\u0000&%\u0001\u0000\u0000\u0000\'*\u0001\u0000"+
		"\u0000\u0000(&\u0001\u0000\u0000\u0000()\u0001\u0000\u0000\u0000)\b\u0001"+
		"\u0000\u0000\u0000*(\u0001\u0000\u0000\u0000+/\u0005[\u0000\u0000,.\t"+
		"\u0000\u0000\u0000-,\u0001\u0000\u0000\u0000.1\u0001\u0000\u0000\u0000"+
		"/0\u0001\u0000\u0000\u0000/-\u0001\u0000\u0000\u000002\u0001\u0000\u0000"+
		"\u00001/\u0001\u0000\u0000\u000023\u0005]\u0000\u00003\n\u0001\u0000\u0000"+
		"\u000048\u0005{\u0000\u000057\t\u0000\u0000\u000065\u0001\u0000\u0000"+
		"\u00007:\u0001\u0000\u0000\u000089\u0001\u0000\u0000\u000086\u0001\u0000"+
		"\u0000\u00009;\u0001\u0000\u0000\u0000:8\u0001\u0000\u0000\u0000;<\u0005"+
		"}\u0000\u0000<\f\u0001\u0000\u0000\u0000=A\u0005\"\u0000\u0000>@\t\u0000"+
		"\u0000\u0000?>\u0001\u0000\u0000\u0000@C\u0001\u0000\u0000\u0000AB\u0001"+
		"\u0000\u0000\u0000A?\u0001\u0000\u0000\u0000BD\u0001\u0000\u0000\u0000"+
		"CA\u0001\u0000\u0000\u0000DE\u0005\"\u0000\u0000E\u000e\u0001\u0000\u0000"+
		"\u0000FG\u0005-\u0000\u0000GH\u0005>\u0000\u0000H\u0010\u0001\u0000\u0000"+
		"\u0000IJ\u0005:\u0000\u0000J\u0012\u0001\u0000\u0000\u0000KL\u0005|\u0000"+
		"\u0000L\u0014\u0001\u0000\u0000\u0000MN\u0005`\u0000\u0000NO\u0005`\u0000"+
		"\u0000OP\u0005`\u0000\u0000PT\u0001\u0000\u0000\u0000QS\t\u0000\u0000"+
		"\u0000RQ\u0001\u0000\u0000\u0000SV\u0001\u0000\u0000\u0000TU\u0001\u0000"+
		"\u0000\u0000TR\u0001\u0000\u0000\u0000UW\u0001\u0000\u0000\u0000VT\u0001"+
		"\u0000\u0000\u0000WX\u0005`\u0000\u0000XY\u0005`\u0000\u0000YZ\u0005`"+
		"\u0000\u0000Z\u0016\u0001\u0000\u0000\u0000[]\u0007\u0003\u0000\u0000"+
		"\\[\u0001\u0000\u0000\u0000]^\u0001\u0000\u0000\u0000^\\\u0001\u0000\u0000"+
		"\u0000^_\u0001\u0000\u0000\u0000_`\u0001\u0000\u0000\u0000`a\u0006\u000b"+
		"\u0000\u0000a\u0018\u0001\u0000\u0000\u0000\b\u0000\"(/8AT^\u0001\u0006"+
		"\u0000\u0000";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}