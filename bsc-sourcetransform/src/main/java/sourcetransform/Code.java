package sourcetransform;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.ast.body.Parameter;
import com.github.javaparser.ast.stmt.*;

public class Code {

    public static NodeList<Statement> before = new NodeList<Statement>(
            //StaticJavaParser.parseStatement("System.gc();"),
            StaticJavaParser.parseStatement("chribirreg = new MetricRegistry();"),
            StaticJavaParser.parseStatement("chribirmGs = new MemoryUsageGaugeSet();"),
            StaticJavaParser.parseStatement("chribirtGs = new ThreadStatesGaugeSet();"),
            StaticJavaParser.parseStatement("chribircGs = new ClassLoadingGaugeSet();"),
            StaticJavaParser.parseStatement("chribirjvmGs = new JvmAttributeGaugeSet();"),
            StaticJavaParser.parseStatement("chribirgMs = new GarbageCollectorMetricSet();"),
            StaticJavaParser.parseStatement("chribirreg.registerAll(chribirmGs);"),
            StaticJavaParser.parseStatement("chribirreg.registerAll(chribirtGs);"),
            StaticJavaParser.parseStatement("chribirreg.registerAll(chribircGs);"),
            StaticJavaParser.parseStatement("chribirreg.registerAll(chribirjvmGs);"),
            StaticJavaParser.parseStatement("chribirreg.registerAll(chribirgMs);"),


            StaticJavaParser.parseStatement("chribiroutputBefore = \"before\";"),
            StaticJavaParser.parseStatement("Iterator<String> chribiriterator = chribirreg.getGauges().keySet().iterator();"),
            new WhileStmt().setCondition(StaticJavaParser.parseExpression("chribiriterator.hasNext()")).setBody(
                    new BlockStmt().setStatements(new NodeList<Statement>(
                            StaticJavaParser.parseStatement("String chribirkey   = chribiriterator.next();"),
                            StaticJavaParser.parseStatement("String chribirvalue = chribirreg.getGauges().get(chribirkey).getValue().toString();"),
                            StaticJavaParser.parseStatement("chribiroutputBefore = chribiroutputBefore + \",\" + chribirvalue;")
                    ))
            )
    );


    public static NodeList<Statement> within = new NodeList<Statement>(
            StaticJavaParser.parseStatement("StackTraceElement[] chribirstacktrace = Thread.currentThread().getStackTrace();"),
            StaticJavaParser.parseStatement("StackTraceElement chribire = chribirstacktrace[1];"),
            StaticJavaParser.parseStatement("String chribirmethodName = chribire.getMethodName();"),
            StaticJavaParser.parseStatement("String chribirclassName = MethodHandles.lookup().lookupClass().getSimpleName();"),
            StaticJavaParser.parseStatement("String chribirpackageName = MethodHandles.lookup().lookupClass().getPackage().getName();"),
            StaticJavaParser.parseStatement("chribiridentifier = \"\\n\" + chribirpackageName + \".\" + chribirclassName + \".\" + chribirmethodName;")
    );


    public static NodeList<Statement> after = new NodeList<Statement>(
            StaticJavaParser.parseStatement("chribiroutputAfter = \"after\";"),
            StaticJavaParser.parseStatement("Iterator<String> chribiriterator = chribirreg.getGauges().keySet().iterator();"),
            new WhileStmt().setCondition(StaticJavaParser.parseExpression("chribiriterator.hasNext()")).setBody(
                    new BlockStmt().setStatements(new NodeList<Statement>(
                            StaticJavaParser.parseStatement("String chribirkey   = chribiriterator.next();"),
                            StaticJavaParser.parseStatement("String chribirvalue = chribirreg.getGauges().get(chribirkey).getValue().toString();"),
                            StaticJavaParser.parseStatement("chribiroutputAfter = chribiroutputAfter + \",\" + chribirvalue;")
                    ))
            ),
            new TryStmt().setTryBlock(new BlockStmt().setStatements(new NodeList<Statement>(
                    StaticJavaParser.parseStatement("BufferedWriter chribircsvWriter = new BufferedWriter(new FileWriter(\""+App.outputMeasurementPath +"\", true));"),
                    StaticJavaParser.parseStatement("chribircsvWriter.append(chribiridentifier+\",\"+\""+App.projectName+"\"+\",\"+\""+App.commit_hash+"\"+\",\"+\""+App.iteration+"\"+\",\"+chribiroutputBefore);"),
                    StaticJavaParser.parseStatement("chribircsvWriter.append(chribiridentifier+\",\"+\""+App.projectName+"\"+\",\"+\""+App.commit_hash+"\"+\",\"+\""+App.iteration+"\"+\",\"+chribiroutputAfter);"),
                    StaticJavaParser.parseStatement("chribircsvWriter.close();")
            ))).setCatchClauses(new NodeList<CatchClause>(new CatchClause().setParameter(new Parameter().setType("Exception").setName("chribirex")))));

}
