
package agent;

import com.codahale.metrics.ConsoleReporter;
import com.codahale.metrics.CsvReporter;
import com.codahale.metrics.JvmAttributeGaugeSet;
import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.jvm.ClassLoadingGaugeSet;
import com.codahale.metrics.jvm.GarbageCollectorMetricSet;
import com.codahale.metrics.jvm.MemoryUsageGaugeSet;
import com.codahale.metrics.jvm.ThreadStatesGaugeSet;
import net.bytebuddy.asm.Advice;
import java.io.FileWriter;
import java.lang.invoke.MethodHandles;
import java.util.Iterator;

public class MyAdvice {
  @Advice.OnMethodEnter
  public static void onEnter(@Advice.Local("reg") MetricRegistry reg, @Advice.Local("rep") ConsoleReporter rep,
                             @Advice.Local("mGs") MemoryUsageGaugeSet mGs, @Advice.Local("tGs") ThreadStatesGaugeSet tGs,
                             @Advice.Local("cGs") ClassLoadingGaugeSet cGs, @Advice.Local("jvmGs") JvmAttributeGaugeSet jvmGs,
                             @Advice.Local("gMs") GarbageCollectorMetricSet gMs, @Advice.Local("csvrep") CsvReporter csvrep,
                             @Advice.Local("identifier") String identifier) {

//    System.out.println("hello from onEnter ...");

//    System.gc();

    reg = new MetricRegistry();
    mGs = new MemoryUsageGaugeSet();
    tGs = new ThreadStatesGaugeSet();
    cGs = new ClassLoadingGaugeSet();
    jvmGs = new JvmAttributeGaugeSet();
    gMs = new GarbageCollectorMetricSet();

    reg.registerAll(mGs);
    reg.registerAll(tGs);
    reg.registerAll(cGs);
    reg.registerAll(jvmGs);
    reg.registerAll(gMs);

    // get test case meta information
    StackTraceElement[] stacktrace = Thread.currentThread().getStackTrace();
    StackTraceElement e = stacktrace[1];//maybe this number needs to be corrected
    String methodName = e.getMethodName();
    String className = MethodHandles.lookup().lookupClass().getSimpleName();
    String packageName = MethodHandles.lookup().lookupClass().getPackage().getName();

    String output = "\n" + packageName + "." + className + "." + methodName;
    identifier = output;
    output = output + ",before";

    Iterator<String> iterator = reg.getGauges().keySet().iterator();
    while(iterator.hasNext()) {
      String key   = iterator.next();
      String value = reg.getGauges().get(key).getValue().toString();
      output = output + "," /*+ "("+key+")"*/ + value;
    }

//    iterator = reg.getMetrics().keySet().iterator();
//    while(iterator.hasNext()) {
//      String key   = iterator.next();
//      String value = reg.getMetrics().get(key).toString();
//      output = output + "," + "("+key+")" + value;
//    }

    // write to file
    try {
      FileWriter csvWriter = new FileWriter("/home/christian/Desktop/data/test.csv", true);
      csvWriter.append(output);
      csvWriter.close();
    } catch (Exception ex) {}

//    System.out.println("end of onmethodenter");
  }

  @Advice.OnMethodExit
  public static void onExit(@Advice.Local("reg") MetricRegistry reg, @Advice.Local("rep") ConsoleReporter rep,
                            @Advice.Local("csvrep") CsvReporter csvrep, @Advice.Local("identifier") String identifier) {

//    System.out.println("hello from onExit ...");

    // get test case meta information
    String output = identifier;
    output = output + ",after";

    Iterator<String> iterator = reg.getGauges().keySet().iterator();
    while(iterator.hasNext()) {
      String key   = iterator.next();
      String value = reg.getGauges().get(key).getValue().toString();
      output = output + ","/* + "("+key+")"*/ + value;
    }

//    iterator = reg.getMetrics().keySet().iterator();
//    while(iterator.hasNext()) {
//      String key   = iterator.next();
//      String value = reg.getMetrics().get(key).toString();
//      output = output + "," + "("+key+")" + value;
//    }

    // write to file
    try {
      FileWriter csvWriter = new FileWriter("/home/christian/Desktop/data/test.csv", true);
      csvWriter.append(output);
      csvWriter.close();
    } catch (Exception ex) {}

//    System.out.println("end of onmethodexit");
  }
}


