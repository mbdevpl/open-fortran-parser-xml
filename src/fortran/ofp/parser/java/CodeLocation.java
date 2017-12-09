package fortran.ofp.parser.java;

public class CodeLocation {

	public int line;
	public int col;

	public CodeLocation(int line, int col) {
		this.line = line;
		this.col = col;
	}

	public String toString() {
		return "@" + line + ":" + col;
	}

}
