package repoOne;
import org.springframework.data.jpa.repository.JpaRepository;

import domainClasses.Test;
import java.util.List;

public interface TestRepoOne extends JpaRepository<Test, Integer>{
	
	List<Test> findByName(String name);
		

}
