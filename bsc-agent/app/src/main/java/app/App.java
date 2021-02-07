package app;


 import com.codahale.metrics.ConsoleReporter;
 import com.codahale.metrics.CsvReporter;
 import com.codahale.metrics.JvmAttributeGaugeSet;
 import com.codahale.metrics.MetricRegistry;
 import com.codahale.metrics.jvm.ClassLoadingGaugeSet;
 import com.codahale.metrics.jvm.GarbageCollectorMetricSet;
 import com.codahale.metrics.jvm.MemoryUsageGaugeSet;
 import com.codahale.metrics.jvm.ThreadStatesGaugeSet;

public class App 
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );

//        MetricRegistry reg = new MetricRegistry();
////        MemoryUsageGaugeSet mGs = new MemoryUsageGaugeSet();
////        ThreadStatesGaugeSet tGs = new ThreadStatesGaugeSet();
////        ClassLoadingGaugeSet cGs = new ClassLoadingGaugeSet();
////        JvmAttributeGaugeSet jvmGs = new JvmAttributeGaugeSet();
//        GarbageCollectorMetricSet gMs = new GarbageCollectorMetricSet();
//
////        reg.registerAll(mGs);
////        reg.registerAll(tGs);
////        reg.registerAll(cGs);
////        reg.registerAll(jvmGs);
//        reg.registerAll(gMs);
//
//        ConsoleReporter rep = ConsoleReporter.forRegistry(reg).build();
//
//        //rep.report();
//


    }
}
