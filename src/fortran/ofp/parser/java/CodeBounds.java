package fortran.ofp.parser.java;

import org.antlr.runtime.Token;
import org.w3c.dom.Element;

public class CodeBounds {

	protected static String Y_MIN = "line_begin";
	protected static String X_MIN = "col_begin";
	protected static String Y_MAX = "line_end";
	protected static String X_MAX = "col_end";

	public CodeLocation begin;
	public CodeLocation end;

	public CodeBounds() {
		begin = null;
		end = null;
	}

	public CodeBounds(int beginLine, int beginCol, int endLine, int endCol) {
		begin = new CodeLocation(beginLine, beginCol);
		end = new CodeLocation(endLine, endCol);
	}

	public CodeBounds(Element context) {
		Integer lineBegin = context.hasAttribute(Y_MIN) ? Integer.valueOf(context.getAttribute(Y_MIN)) : null;
		Integer colBegin = context.hasAttribute(X_MIN) ? Integer.valueOf(context.getAttribute(X_MIN)) : null;
		if (lineBegin == null && colBegin == null)
			begin = null;
		else if (lineBegin == null || colBegin == null)
			throw new IllegalArgumentException("the implementation of this method is all-or-nothing");
		else
			begin = new CodeLocation(lineBegin, colBegin);
		Integer lineEnd = context.hasAttribute(Y_MAX) ? Integer.valueOf(context.getAttribute(Y_MAX)) : null;
		Integer colEnd = context.hasAttribute(X_MAX) ? Integer.valueOf(context.getAttribute(X_MAX)) : null;
		if (lineEnd == null && colEnd == null)
			begin = null;
		else if (lineEnd == null || colEnd == null)
			throw new IllegalArgumentException("the implementation of this method is all-or-nothing");
		else
		end = new CodeLocation(lineEnd, colEnd);
	}

	public CodeBounds(Token token) {
		Integer line = token.getLine();
		Integer colBegin = token.getCharPositionInLine();
		Integer colEnd = colBegin + token.getText().length();
		begin = new CodeLocation(line, colBegin);
		end = new CodeLocation(line, colEnd);
	}

	public void extend(CodeLocation loc) {
		if (loc == null)
			throw new IllegalArgumentException("the implementation of this method is all-or-nothing");

		if (begin == null && end == null) {
			begin = new CodeLocation(loc.line, loc.col);
			end = new CodeLocation(loc.line, loc.col);
			return;
		} else if (begin == null || end == null)
			throw new IllegalArgumentException("the implementation of this method is all-or-nothing");

		boolean updateLineBegin = loc.line < begin.line;
		boolean updateColBegin = updateLineBegin || loc.line == begin.line && loc.col < begin.col;
		boolean updateLineEnd = loc.line > end.line;
		boolean updateColEnd = updateLineEnd || loc.line == end.line && loc.col > end.col;

		if (updateLineBegin)
			begin.line = loc.line;
		if (updateColBegin)
			begin.col = loc.col;
		if (updateLineEnd)
			end.line = loc.line;
		if (updateColEnd)
			end.col = loc.col;
	}

	public void extend(Token token) {
		CodeBounds bounds = new CodeBounds(token);
		extend(bounds.begin);
		extend(bounds.end);
	}

	public void persist(Element context) {
		context.setAttribute(Y_MIN, Integer.toString(begin.line));
		context.setAttribute(X_MIN, Integer.toString(begin.col));
		context.setAttribute(Y_MAX, Integer.toString(end.line));
		context.setAttribute(X_MAX, Integer.toString(end.col));
	}

	public String toString() {
		return begin + "~" + end;
	}

}
