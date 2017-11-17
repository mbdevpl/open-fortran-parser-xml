package fortran.ofp.parser.java;

import org.antlr.runtime.Lexer;
import org.antlr.runtime.Token;

public class FortranCommentLexer extends FortranLexer {

  public FortranCommentLexer(FortranStream stream) {
    super(stream);
  }

  public Token nextToken() {
    Token tk = super.nextToken();
    //Lexer lexer = (Lexer) this;
    //Token tk = lexer.nextToken();
    return tk;
  }

}
