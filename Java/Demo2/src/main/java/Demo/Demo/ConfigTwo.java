package Demo.Demo;

import org.springframework.context.annotation.Configuration;
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
		basePackages = "repoTwo",
		entityManagerFactoryRef = "secondEntityManagerFactory", 
	    transactionManagerRef = "secondTransactionManager"
		)
public class ConfigTwo {
	
	
	   @Bean
	   public LocalContainerEntityManagerFactoryBean secondEntityManagerFactory() {
	      LocalContainerEntityManagerFactoryBean em = new LocalContainerEntityManagerFactoryBean();
	      em.setDataSource(dataSourceTwo());
	      em.setPackagesToScan(new String[] { "repoTwo", "domainClasses" });
	 
	      JpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
	      em.setJpaVendorAdapter(vendorAdapter);
	      em.setJpaProperties(additionalProperties());
	      
	 
	      return em;
	   }
	 
	   @Bean
	   public DataSource dataSourceTwo(){
	      DriverManagerDataSource dataSource = new DriverManagerDataSource();
	      dataSource.setDriverClassName("org.postgresql.Driver");
	      dataSource.setUrl("jdbc:postgresql://localhost:5432/testtwo");
	      dataSource.setUsername( "master" );
	      dataSource.setPassword( "password" );
	      return dataSource;
	   }
	 
	   @Bean
	   public PlatformTransactionManager secondTransactionManager(EntityManagerFactory emf){
	      JpaTransactionManager transactionManager = new JpaTransactionManager();
	      transactionManager.setEntityManagerFactory(emf);
	 
	      return transactionManager;
	   }
	 
	   @Bean
	   public PersistenceExceptionTranslationPostProcessor exceptionTranslationTwo(){
	      return new PersistenceExceptionTranslationPostProcessor();
	   }
	   
	   Properties additionalProperties() {
		      Properties properties = new Properties();
		     //properties.setProperty("hibernate.hbm2ddl.auto", "create-drop");
		      properties.setProperty("hibernate.dialect", "org.hibernate.dialect.PostgreSQLDialect");
		      return properties;
		   }
	 
	

}
