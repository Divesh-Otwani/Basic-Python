package webservices;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import repoOne.TestRepoOne;
import repoTwo.TestRepoTwo;

/*
 * this site:
 * 
 * http://localhost:8080/name/{name}
 * 
 * 
 * 
 */


@RestController
@RequestMapping("/name/{name}")
public class QueryByName {
	
	@Autowired
	TestRepoOne repo;
	
	@Autowired
	TestRepoTwo repo2;
	
	
	
	@RequestMapping(method = RequestMethod.GET)
	public String stuff(@PathVariable("name") String name){
		return "repo one: " 
				+ repo.findByName(name).toString() 
				+ "  \nrepo two: " 
				+ repo2.findByName(name).toString();
	}
	
	
}
