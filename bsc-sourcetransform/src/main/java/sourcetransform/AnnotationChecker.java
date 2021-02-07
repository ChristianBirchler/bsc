package sourcetransform;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.stmt.*;
import com.github.javaparser.ast.visitor.ModifierVisitor;
import com.github.javaparser.ast.visitor.Visitable;

public class AnnotationChecker extends ModifierVisitor<Void> {

    TestData testData;

    public AnnotationChecker(TestData testData) {
        super();
        this.testData = testData;
    }

    @Override
    public Visitable visit(MethodDeclaration md, Void arg) {
        super.visit(md, arg);
        if (md.getAnnotationByName("Before").isPresent()) {
            testData.hasBefore = true;
            appendBefore(md);
        } else if (md.getAnnotationByName("After").isPresent()) {
            testData.hasAfter = true;
            appendAfter(md);
        } else if (md.getAnnotationByName("Test").isPresent()) {
            appendWithin(md);
        }
        //addBefore(md);
        //addAfter(md);
        //addMetricsInitAndWriteStatements(md);
        return md;
    }



    private void appendBefore(MethodDeclaration md) {
        if (md.getBody().isPresent()) {
            md.getBody().get().getStatements().addAll(Code.before);
        } else {
            md.setBody(new BlockStmt().setStatements(Code.before));
        }
    }

    private void appendWithin(MethodDeclaration md) {
        if (!md.getBody().isPresent()) return;
        md.getBody().get().getStatements().addAll(0, Code.within);
    }

    private void appendAfter(MethodDeclaration md) {
        if (md.getBody().isPresent()) {
            md.getBody().get().getStatements().addAll(0, Code.after);
        } else {
            md.setBody(new BlockStmt().setStatements(Code.before));
        }
    }











    private void addMetricsInitAndWriteStatements(MethodDeclaration md) {

        if (!md.getBody().isPresent()) return;


        Statement returnStmt = null;

        if (md.getBody().get().getStatements().getLast().isPresent() &&
                md.getBody().get().getStatements().getLast().get().isReturnStmt()) {
            returnStmt = md.getBody().get().getStatements().removeLast();
        }

        NodeList<Statement> oldStmts = md.getBody().get().getStatements();

        md.setBody(new BlockStmt().setStatements(
                new NodeList<Statement>(
                        //StaticJavaParser.parseStatement("System.gc();"),
                        StaticJavaParser.parseStatement("MetricRegistry chribirreg = new MetricRegistry();"),
                        StaticJavaParser.parseStatement("MemoryUsageGaugeSet chribirmGs = new MemoryUsageGaugeSet();"),
                        StaticJavaParser.parseStatement("ThreadStatesGaugeSet chribirtGs = new ThreadStatesGaugeSet();"),
                        StaticJavaParser.parseStatement("ClassLoadingGaugeSet chribircGs = new ClassLoadingGaugeSet();"),
                        StaticJavaParser.parseStatement("JvmAttributeGaugeSet chribirjvmGs = new JvmAttributeGaugeSet();"),
                        StaticJavaParser.parseStatement("GarbageCollectorMetricSet chribirgMs = new GarbageCollectorMetricSet();"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribirmGs);"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribirtGs);"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribircGs);"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribirjvmGs);"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribirgMs);"),
                        StaticJavaParser.parseStatement("StackTraceElement[] chribirstacktrace = Thread.currentThread().getStackTrace();"),
                        StaticJavaParser.parseStatement("StackTraceElement chribire = chribirstacktrace[1];"),
                        StaticJavaParser.parseStatement("String chribirmethodName = chribire.getMethodName();"),
                        StaticJavaParser.parseStatement("String chribirclassName = MethodHandles.lookup().lookupClass().getSimpleName();"),
                        StaticJavaParser.parseStatement("String chribirpackageName = MethodHandles.lookup().lookupClass().getPackage().getName();"),
                        StaticJavaParser.parseStatement("String chribiroutput = \"\\n\" + chribirpackageName + \".\" + chribirclassName + \".\" + chribirmethodName;"),
                        StaticJavaParser.parseStatement("String chribiridentifier = chribiroutput;"),
                        StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",before\";"),
                        StaticJavaParser.parseStatement("boolean chribirsuccess = true;"),
                        StaticJavaParser.parseStatement("Iterator<String> chribiriterator = chribirreg.getGauges().keySet().iterator();"),
                        new WhileStmt().setCondition(StaticJavaParser.parseExpression("chribiriterator.hasNext()")).setBody(
                                new BlockStmt().setStatements(new NodeList<Statement>(
                                        StaticJavaParser.parseStatement("String chribirkey   = chribiriterator.next();"),
                                        StaticJavaParser.parseStatement("String chribirvalue = chribirreg.getGauges().get(chribirkey).getValue().toString();"),
                                        StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",\" + chribirvalue;")
                                ))
                        ),
                        new TryStmt().setTryBlock(new BlockStmt().setStatements(new NodeList<Statement>(
                                StaticJavaParser.parseStatement("FileWriter chribircsvWriter = new FileWriter(\""+App.outputMeasurementPath +"\", true);"),
                                StaticJavaParser.parseStatement("chribircsvWriter.append(chribiroutput+\",\"+"+App.iteration+"+\",U\");"),
                                StaticJavaParser.parseStatement("chribircsvWriter.close();")
                        ))).setCatchClauses(new NodeList<CatchClause>(new CatchClause().setParameter(new Parameter().setType("Exception").setName("chribirex"))))
                )));

        // try/catch clause for AssertionError --> test fails
        md.getBody().get().getStatements().add(new TryStmt().setTryBlock(new BlockStmt().setStatements(oldStmts))
                .setCatchClauses(new NodeList<CatchClause>(
                        new CatchClause().setParameter(new Parameter().setType("AssertionError").setName("chribirerror")
                        ).setBody(new BlockStmt().setStatements(new NodeList<Statement>(
                                StaticJavaParser.parseStatement("chribirsuccess = false;"),
                                StaticJavaParser.parseStatement("chribiroutput = chribiridentifier;"),
                                StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",after\";"),
                                StaticJavaParser.parseStatement("chribiriterator = chribirreg.getGauges().keySet().iterator();"),
                                new WhileStmt().setCondition(StaticJavaParser.parseExpression("chribiriterator.hasNext()")).setBody(
                                        new BlockStmt().setStatements(new NodeList<Statement>(
                                                StaticJavaParser.parseStatement("String chribirkey   = chribiriterator.next();"),
                                                StaticJavaParser.parseStatement("String chribirvalue = chribirreg.getGauges().get(chribirkey).getValue().toString();"),
                                                StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",\" + chribirvalue;")
                                        ))
                                ),
                                new TryStmt().setTryBlock(new BlockStmt().setStatements(new NodeList<Statement>(
                                        StaticJavaParser.parseStatement("FileWriter chribircsvWriter = new FileWriter(\""+App.outputMeasurementPath +"\", true);"),
                                        StaticJavaParser.parseStatement("chribircsvWriter.append(chribiroutput+\",\"+"+App.iteration+"+\",F\");"),
                                        StaticJavaParser.parseStatement("chribircsvWriter.close();")
                                ))).setCatchClauses(new NodeList<CatchClause>(new CatchClause().setParameter(new Parameter().setType("Exception").setName("chribirex"))))
                        ))))));

        // writing if test passes

        md.getBody().get().getStatements().add(new IfStmt()
                .setCondition(StaticJavaParser.parseExpression("chribirsuccess")).setThenStmt(new BlockStmt().setStatements(
                        new NodeList<Statement>(
                                StaticJavaParser.parseStatement("chribiroutput = chribiridentifier;"),
                                StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",after\";"),
                                StaticJavaParser.parseStatement("chribiriterator = chribirreg.getGauges().keySet().iterator();"),
                                new WhileStmt().setCondition(StaticJavaParser.parseExpression("chribiriterator.hasNext()")).setBody(
                                        new BlockStmt().setStatements(new NodeList<Statement>(
                                                StaticJavaParser.parseStatement("String chribirkey   = chribiriterator.next();"),
                                                StaticJavaParser.parseStatement("String chribirvalue = chribirreg.getGauges().get(chribirkey).getValue().toString();"),
                                                StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",\" + chribirvalue;")
                                        ))
                                ),
                                new TryStmt().setTryBlock(new BlockStmt().setStatements(new NodeList<Statement>(
                                        StaticJavaParser.parseStatement("FileWriter chribircsvWriter = new FileWriter(\""+App.outputMeasurementPath +"\", true);"),
                                        StaticJavaParser.parseStatement("chribircsvWriter.append(chribiroutput+\",\"+"+App.iteration+"+\",P\");"),
                                        StaticJavaParser.parseStatement("chribircsvWriter.close();")
                                ))).setCatchClauses(new NodeList<CatchClause>(new CatchClause().setParameter(new Parameter().setType("Exception").setName("chribirex")))))
                )));

        if (returnStmt != null) md.getBody().get().getStatements().addLast(returnStmt);
    }





    private void addBefore(MethodDeclaration md) {

        if (!md.getBody().isPresent()) return;

        NodeList<Statement> oldStmts = md.getBody().get().getStatements();

        md.setBody(new BlockStmt().setStatements(
                new NodeList<Statement>(
                        //StaticJavaParser.parseStatement("System.gc();"),
                        StaticJavaParser.parseStatement("MetricRegistry chribirreg = new MetricRegistry();"),
                        StaticJavaParser.parseStatement("MemoryUsageGaugeSet chribirmGs = new MemoryUsageGaugeSet();"),
                        StaticJavaParser.parseStatement("ThreadStatesGaugeSet chribirtGs = new ThreadStatesGaugeSet();"),
                        StaticJavaParser.parseStatement("ClassLoadingGaugeSet chribircGs = new ClassLoadingGaugeSet();"),
                        StaticJavaParser.parseStatement("JvmAttributeGaugeSet chribirjvmGs = new JvmAttributeGaugeSet();"),
                        StaticJavaParser.parseStatement("GarbageCollectorMetricSet chribirgMs = new GarbageCollectorMetricSet();"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribirmGs);"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribirtGs);"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribircGs);"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribirjvmGs);"),
                        StaticJavaParser.parseStatement("chribirreg.registerAll(chribirgMs);"),
                        StaticJavaParser.parseStatement("StackTraceElement[] chribirstacktrace = Thread.currentThread().getStackTrace();"),
                        StaticJavaParser.parseStatement("StackTraceElement chribire = chribirstacktrace[1];"),
                        StaticJavaParser.parseStatement("String chribirmethodName = chribire.getMethodName();"),
                        StaticJavaParser.parseStatement("String chribirclassName = MethodHandles.lookup().lookupClass().getSimpleName();"),
                        StaticJavaParser.parseStatement("String chribirpackageName = MethodHandles.lookup().lookupClass().getPackage().getName();"),
                        StaticJavaParser.parseStatement("String chribiroutput = \"\\n\" + chribirpackageName + \".\" + chribirclassName + \".\" + chribirmethodName;"),
                        StaticJavaParser.parseStatement("String chribiridentifier = chribiroutput;"),
                        StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",before\";"),
                        StaticJavaParser.parseStatement("Iterator<String> chribiriterator = chribirreg.getGauges().keySet().iterator();"),
                        new WhileStmt().setCondition(StaticJavaParser.parseExpression("chribiriterator.hasNext()")).setBody(
                                new BlockStmt().setStatements(new NodeList<Statement>(
                                        StaticJavaParser.parseStatement("String chribirkey   = chribiriterator.next();"),
                                        StaticJavaParser.parseStatement("String chribirvalue = chribirreg.getGauges().get(chribirkey).getValue().toString();"),
                                        StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",\" + chribirvalue;")
                                ))
                        ),
                        new TryStmt().setTryBlock(new BlockStmt().setStatements(new NodeList<Statement>(
                                StaticJavaParser.parseStatement("FileWriter chribircsvWriter = new FileWriter(\"/home/christian/Desktop/data/test.csv\", true);"),
                                StaticJavaParser.parseStatement("chribircsvWriter.append(chribiroutput);"),
                                StaticJavaParser.parseStatement("chribircsvWriter.close();")
                        ))).setCatchClauses(new NodeList<CatchClause>(new CatchClause().setParameter(new Parameter().setType("Exception").setName("chribirex"))))
                        )));

        // append the old method body
        md.getBody().get().getStatements().addAll(oldStmts);
    }

    private void addAfter(MethodDeclaration md) {

        if (!md.getBody().isPresent()) return;

        NodeList<Statement> oldStmts = md.getBody().get().getStatements();

        Statement returnStmt = null;
        if (oldStmts.getLast().isPresent() && oldStmts.getLast().get().isReturnStmt()) {
            returnStmt = oldStmts.removeLast();
        }

        oldStmts.addAll(new NodeList<Statement>(
                StaticJavaParser.parseStatement("chribiroutput = chribiridentifier;"),
                StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",after\";"),
                StaticJavaParser.parseStatement("chribiriterator = chribirreg.getGauges().keySet().iterator();"),
                new WhileStmt().setCondition(StaticJavaParser.parseExpression("chribiriterator.hasNext()")).setBody(
                        new BlockStmt().setStatements(new NodeList<Statement>(
                                StaticJavaParser.parseStatement("String chribirkey   = chribiriterator.next();"),
                                StaticJavaParser.parseStatement("String chribirvalue = chribirreg.getGauges().get(chribirkey).getValue().toString();"),
                                StaticJavaParser.parseStatement("chribiroutput = chribiroutput + \",\" + chribirvalue;")
                        ))
                ),
                new TryStmt().setTryBlock(new BlockStmt().setStatements(new NodeList<Statement>(
                        StaticJavaParser.parseStatement("FileWriter chribircsvWriter = new FileWriter(\"/home/christian/Desktop/data/test.csv\", true);"),
                        StaticJavaParser.parseStatement("chribircsvWriter.append(chribiroutput);"),
                        StaticJavaParser.parseStatement("chribircsvWriter.close();")
                ))).setCatchClauses(new NodeList<CatchClause>(new CatchClause().setParameter(new Parameter().setType("Exception").setName("chribirex"))))
        ));

        if (returnStmt != null) oldStmts.addLast(returnStmt);
    }
}

