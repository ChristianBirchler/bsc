package sourcetransform;

import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.expr.AnnotationExpr;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.github.javaparser.ast.visitor.ModifierVisitor;
import com.github.javaparser.ast.visitor.Visitable;

public class TestClassChecker extends ModifierVisitor<Void> {
    /*
    This class is a visitor which checks if the current compilation
    unit inherits from junit TestCase.
     */

    private TestData testData;

    public TestClassChecker(TestData testData) {
        super();
        this.testData = testData;
    }

    @Override
    public Visitable visit(ClassOrInterfaceDeclaration n, Void arg) {
        super.visit(n, arg);

        for (ClassOrInterfaceType type : n.getExtendedTypes()) {
            if (type.getName().toString().contains("TestCase")) {
                testData.extendsTestCase = true;
                testData.isTestClass = true;
            }
        }

        return n;
    }

    @Override
    public Visitable visit(MethodDeclaration md, Void arg) {
        super.visit(md, arg);
        if (md.getAnnotationByName("Test").isPresent()){
            testData.isTestClass = true;
            testData.useAnnotations = true;
        }
        return md;
    }
}
