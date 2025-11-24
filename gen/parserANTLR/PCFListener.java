// Generated from /Users/firasbouzazi/Documents/TP1-3_Final/src/parserANTLR/PCF.g4 by ANTLR 4.13.2
package parser;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link PCFParser}.
 */
public interface PCFListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code Number}
	 * labeled alternative in {@link PCFParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterNumber(PCFParser.NumberContext ctx);
	/**
	 * Exit a parse tree produced by the {@code Number}
	 * labeled alternative in {@link PCFParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitNumber(PCFParser.NumberContext ctx);
	/**
	 * Enter a parse tree produced by the {@code BinaryTerm}
	 * labeled alternative in {@link PCFParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterBinaryTerm(PCFParser.BinaryTermContext ctx);
	/**
	 * Exit a parse tree produced by the {@code BinaryTerm}
	 * labeled alternative in {@link PCFParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitBinaryTerm(PCFParser.BinaryTermContext ctx);
	/**
	 * Enter a parse tree produced by the {@code IfZero}
	 * labeled alternative in {@link PCFParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterIfZero(PCFParser.IfZeroContext ctx);
	/**
	 * Exit a parse tree produced by the {@code IfZero}
	 * labeled alternative in {@link PCFParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitIfZero(PCFParser.IfZeroContext ctx);
}