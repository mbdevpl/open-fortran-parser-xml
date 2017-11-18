package fortran.ofp.parser.java;

import java.io.File;

import org.antlr.runtime.Lexer;
import org.antlr.runtime.Token;

public class FortranCommentLexer extends FortranAlternateLexer {

  public FortranCommentLexer(FortranStream stream) {
    super(stream);
    throw new RuntimeException();
  }

  private File findFile(String fileName) {
    return null;
  }

  public Token nextToken() {
    Token tk = super.nextToken();
    //Lexer lexer = (Lexer) this;
    //Token tk = lexer.nextToken();
    return tk;
  }

}
