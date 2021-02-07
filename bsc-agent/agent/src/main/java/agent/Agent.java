
package agent;

import net.bytebuddy.agent.builder.AgentBuilder;
import net.bytebuddy.asm.Advice;
import net.bytebuddy.matcher.ElementMatchers;
import org.junit.Test;

import java.lang.instrument.Instrumentation;

public class Agent {
    public static void premain(String args, Instrumentation inst) {
        // System.out.println("run premain ...");
        new AgentBuilder.Default()
            .type(ElementMatchers.any())
            .transform((builder, type, classLoader, module) -> builder
                .visit(Advice.to(MyAdvice.class).on(ElementMatchers.isMethod()
                                                    .and(ElementMatchers.isAnnotatedWith(Test.class))
                                                    )
                )
            )
            .installOn(inst);
    }
}
