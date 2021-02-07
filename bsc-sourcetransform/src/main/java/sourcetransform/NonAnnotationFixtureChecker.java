package sourcetransform;

import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.visitor.ModifierVisitor;
import com.github.javaparser.ast.visitor.Visitable;

public class NonAnnotationFixtureChecker extends ModifierVisitor<Void> {

    private TestData testData;

    public NonAnnotationFixtureChecker(TestData testData) {
        super();
        this.testData = testData;
    }

    @Override
    public Visitable visit(MethodDeclaration md, Void arg) {
        super.visit(md, arg);

        // is the setUp method
        if (md.getName().toString().equals("setUp")){
            testData.hasSetup = true;
            appendSetUp(md);
        }

        // is the tearDown method
        else if (md.getName().toString().equals("tearDown")) {
            testData.hasTearDown = true;
            appendTearDown(md);
        }

        // is the test case
        else if (md.getName().toString().startsWith("test")) {
            appendWithin(md);
        }

        return md;
    }

    private void appendWithin(MethodDeclaration md) {
        if (md.getBody().isPresent()) {
            md.getBody().get().getStatements().addAll(0, Code.within);
        } else {
            md.setBody(new BlockStmt().setStatements(Code.within));
        }
    }

    private void appendTearDown(MethodDeclaration md) {
        if (md.getBody().isPresent()) {
            md.getBody().get().getStatements().addAll(0, Code.after);
        } else {
            md.setBody(new BlockStmt().setStatements(Code.after));
        }
    }

    private void appendSetUp(MethodDeclaration md) {
        if (md.getBody().isPresent()){
            md.getBody().get().getStatements().addAll(Code.before);
        } else {
            md.setBody(new BlockStmt().setStatements(Code.before));
        }
    }
}
