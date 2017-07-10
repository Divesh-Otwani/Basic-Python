package Demo.Demo;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import java.util.Properties;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;


import org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor;
import org.springframework.context.annotation.Bean;

import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.JpaVendorAdapter;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;

/*
 * Comments: 
 * 
 * 
 * 
 */


@Configuration
@EnableJpaRepositories(
		basePackages = "repoOne",
		entityManagerFactoryRef = "firstEntityManagerFactory", 
	    transactionManagerRef = "firstTransactionManager"
		)
public class ConfigOne {
	
	
	   @Bean
	   @Primary
	   public LocalContainerEntityManagerFactoryBean firstEntityManagerFactory() {
	      LocalContainerEntityManagerFactoryBean em = new LocalContainerEntityManagerFactoryBean();
	      em.setDataSource(dataSource());
	      em.setPackagesToScan(new String[] { "repoOne", "domainClasses" });
	 
	      JpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
	      em.setJpaVendorAdapter(vendorAdapter);
	      em.setJpaProperties(additionalProperties());
	      
	 
	      return em;
	   }
	 
	   @Bean
	   @Primary
	   public DataSource dataSource(){
	      DriverManagerDataSource dataSource = new DriverManagerDataSource();
	      dataSource.setDriverClassName("org.postgresql.Driver");
	      dataSource.setUrl("jdbc:postgresql://localhost:5432/testone");
	      dataSource.setUsername( "master" );
	      dataSource.setPassword( "password" );
	      return dataSource;
	   }
	 
	   @Bean
	   @Primary
	   public PlatformTransactionManager firstTransactionManager(EntityManagerFactory emf){
	      JpaTransactionManager transactionManager = new JpaTransactionManager();
	      transactionManager.setEntityManagerFactory(emf);
	 
	      return transactionManager;
	   }
	 
	   @Bean
	   @Primary
	   public PersistenceExceptionTranslationPostProcessor exceptionTranslation(){
	      return new PersistenceExceptionTranslationPostProcessor();
	   }
	   
	   Properties additionalProperties() {
		      Properties properties = new Properties();
		     //properties.setProperty("hibernate.hbm2ddl.auto", "create-drop");
		      properties.setProperty("hibernate.dialect", "org.hibernate.dialect.PostgreSQLDialect");
		      return properties;
		   }
	 
	

}
