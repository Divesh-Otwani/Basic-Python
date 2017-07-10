package webservices;



//import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;


/*
 * this site:
 * 
 * http://localhost:8080/helloworld
 * 
 */


@RestController
@RequestMapping(path = "/helloworld")
public class HelloWorld {

	@RequestMapping(method = RequestMethod.GET)
	public String test(){
		return "This is my test";
	}
	
}
