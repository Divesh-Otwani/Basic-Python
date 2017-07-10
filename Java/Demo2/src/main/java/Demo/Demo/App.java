package Demo.Demo;

import java.io.File;

import org.springframework.boot.SpringApplication;

import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.sun.codemodel.JAnnotationUse;
import com.sun.codemodel.JCodeModel;
import com.sun.codemodel.JDefinedClass;
import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JMod;

@SpringBootApplication
@ComponentScan({ "Demo.Demo", "webservices" })
public class App {

	public static void main(String[] args) throws Exception {

		JCodeModel jm = new JCodeModel();
		JDefinedClass dc;

		dc = jm._class("webservices.Bar");
		dc.annotate(RestController.class);
		JAnnotationUse t = dc.annotate(RequestMapping.class);
		t.param("path", "/testing");

		JMethod m = dc.method(JMod.PUBLIC, String.class, "method");
		JAnnotationUse use = m.annotate(RequestMapping.class);
		use.param("method", RequestMethod.GET);
		m.body()._return(JExpr.lit("This actually worked!!!"));

		jm.build(new File("/home/divesh/Documents/workspace-sts-3.8.4.RELEASE/Demo2/src/main/java"));
		System.out.println(dc.toString());

		SpringApplication.run(App.class, args);

	}
}
