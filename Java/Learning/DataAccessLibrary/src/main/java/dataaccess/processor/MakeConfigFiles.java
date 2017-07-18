package dataaccess.processor;

import java.io.Writer;
//import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.annotation.processing.Filer;
import javax.lang.model.element.Modifier;
import javax.tools.JavaFileObject;

import com.squareup.javawriter.JavaWriter;

import dataaccess.annotation.database.DataAccessConfig;
import dataaccess.annotation.database.DataBaseConn;

public class MakeConfigFiles {
	
	private static String[] imports = {
			"org.springframework.context.annotation.Configuration",
			"org.springframework.context.annotation.Primary",
			"org.springframework.data.jpa.repository.config.EnableJpaRepositories",
			"java.util.Properties",
			"javax.persistence.EntityManagerFactory",
			"javax.sql.DataSource",
			"org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor",
			"org.springframework.context.annotation.Bean",
			"org.springframework.jdbc.datasource.DriverManagerDataSource",
			"org.springframework.orm.jpa.JpaTransactionManager",
			"org.springframework.orm.jpa.JpaVendorAdapter",
			"org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean",
			"org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter",
			"org.springframework.transaction.PlatformTransactionManager"
	};
	
	// MUST format strings before writing...
	private static String[] methodOne = { 
		  "LocalContainerEntityManagerFactoryBean em = new LocalContainerEntityManagerFactoryBean()",
	      "em.setDataSource(%s)",
	      "em.setPackagesToScan(new String[] { \"%s\", \"%s\" })",
	      "JpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter()",
	      "em.setJpaVendorAdapter(vendorAdapter)",
	      "em.setJpaProperties(additionalProperties())",
	      "return em"
		};

	private static String[] methodTwo = {
		  "DriverManagerDataSource dataSource = new DriverManagerDataSource()",
	      "dataSource.setDriverClassName(\"%s\")",
	      "dataSource.setUrl(\"%s\")",
	      "dataSource.setUsername(\"%s\")",
	      "dataSource.setPassword(\"%s\")",
	      "return dataSource"
	};
	
	private static String[] methodThree = {
		  "JpaTransactionManager transactionManager = new JpaTransactionManager()",
	      "transactionManager.setEntityManagerFactory(emf)",
	      "return transactionManager"
	};
	
	private static String[] methodFour = {
	      "return new PersistenceExceptionTranslationPostProcessor()"
	};

	private static String[] methodFive = {
		   "Properties properties = new Properties()",
		   "properties.setProperty(\"hibernate.dialect\", \"org.hibernate.dialect.PostgreSQLDialect\")",
		   "return properties"
	};
	 

	public 
	static HashMap<String, DataBaseConn> createConfigFiles(
			DataAccessConfig cf, 
			String pkgNm, 
			String clsNmPrefix,
			Filer f) throws Exception{
		HashMap<String, DataBaseConn> uidMap = 
				new HashMap<String, DataBaseConn>();
		
		//make primary
		String primaryUid = cf.primaryDb().uniqueId();
		uidMap.put(primaryUid, cf.primaryDb());
		String srcNm = 
				pkgNm + "." +
				clsNmPrefix + 
				primaryUid + 
				"Config";
		JavaFileObject file = f.createSourceFile(srcNm);
		makeConfig(
				file, 
				clsNmPrefix, 
				pkgNm, 
				cf.primaryDb(), 
				true);
		
		//make rest
		DataBaseConn[] otherDbs = cf.secondaryDbs();
		for (DataBaseConn db : otherDbs){
			String uid = db.uniqueId();
			uidMap.put(uid, db);
			String srcFileNm = 
					pkgNm + "." +
					clsNmPrefix +
					uid +
					"Config";
			JavaFileObject afile = 
					f.createSourceFile(srcFileNm);
			makeConfig(
					afile,
					clsNmPrefix,
					pkgNm, 
					db,
					false);
		}
		return uidMap;
		
	}

	private static void 
	makeConfig(JavaFileObject file, 
			String clsNmPrefix, 
			String pkgNm, 
			DataBaseConn dcon,
			boolean isPrimary) throws Exception {
		
		String uid = dcon.uniqueId();
		String clNm = clsNmPrefix + uid + "Config";
		
		// Method names
		String emfNm = uid + "emf";
		String tmrNm = uid + "tmr";
		String extNm = uid + "ext";
		String dtaNm = uid + "dts";
		
		Writer w = file.openWriter();
		JavaWriter jw = new JavaWriter(w);
		jw.emitPackage(pkgNm);
		jw.emitImports(imports);
		jw.emitAnnotation("Configuration");
		HashMap<String, String> jpaArgs =
				new HashMap<String, String>();
		jpaArgs.put("basePackages", quote(dcon.repoPkgNm()));
		jpaArgs.put("entityManagerFactoryRef", quote(emfNm));
		jpaArgs.put("transactionManagerRef", quote(tmrNm));
		jw.emitAnnotation("EnableJpaRepositories", jpaArgs);
		jw.beginType(clNm, "class", getMods());
		
		String[] methAns = 
				(isPrimary) ? 
						new String[] {"Bean", "Primary"} : 
						new String[] {"Bean"};
		
		jw.emitEmptyLine();
		createFn1(jw, dcon, methAns, emfNm, dtaNm);
		jw.emitEmptyLine();
		createFn2(jw, dcon, methAns, dtaNm);
		jw.emitEmptyLine();
		createFn3(jw, dcon, methAns, tmrNm);
		jw.emitEmptyLine();
		createFn4(jw, dcon, methAns, extNm);
		jw.emitEmptyLine();
		createFn5(jw, dcon);
		
		jw.endType();
		jw.close();
	
	}
	private static void makeAnns(
			JavaWriter jw, 
			String[] ans) throws Exception {
		for (String an : ans){
			jw.emitAnnotation(an);
		}
	}

	private static void createFn1(
			JavaWriter jw, 
			DataBaseConn dcon, 
			String[] methAns, 
			String methodNm,
			String dSrcMethNm) throws Exception{
		
		makeAnns(jw, methAns);
		
		String retTp = "LocalContainerEntityManagerFactoryBean";
		jw.beginMethod(retTp, methodNm, getMods());
		
		int len = methodOne.length;
		for(int i = 0; i < len; ++i){
			if (i == 1){
				String stm = String.format(
						methodOne[i], 
						dSrcMethNm + "()");
				jw.emitStatement(stm);
			}else if(i == 2){
				String stm = String.format(
						methodOne[i],
						dcon.repoPkgNm(),
						dcon.domainPkgNm());
				jw.emitStatement(stm);
			}else{
				jw.emitStatement(methodOne[i]);
			}
		}
		
		jw.endMethod();
	}

	private static void createFn2(
			JavaWriter jw, 
			DataBaseConn dcon, 
			String[] methAns, 
			String methodNm) throws Exception {
		
		makeAnns(jw, methAns);
		String retTp = "DataSource";
		jw.beginMethod(retTp, methodNm, getMods());
		int len = methodTwo.length;
		for(int i = 0; i < len; ++i){
			if (i == 1){
				String stm = String.format(
						methodTwo[i],
						dcon.dbDriverNm());
				jw.emitStatement(stm);
			}else if (i == 2){
				String stm = String.format(
						methodTwo[i],
						dcon.url());
				jw.emitStatement(stm);
			}else if (i == 3){
				String stm = String.format(
						methodTwo[i],
						dcon.username());
				jw.emitStatement(stm);
			}else if (i == 4){
				String stm = String.format(
						methodTwo[i],
						dcon.password());
				jw.emitStatement(stm);
			}else{
				jw.emitStatement(methodTwo[i]);
			}
		}
		jw.endMethod();
	}

	private static void createFn3(
			JavaWriter jw, 
			DataBaseConn dcon, 
			String[] methAns, 
			String methodNm) throws Exception{
		makeAnns(jw, methAns);
		String retTp = "PlatformTransactionManager";
		String args[] = {"EntityManagerFactory", "emf"};
		jw.beginMethod(retTp, methodNm, getMods(), args);
		for (String stm : methodThree){
			jw.emitStatement(stm);
		}
		jw.endMethod();
	}

	private static void createFn4(
			JavaWriter jw, 
			DataBaseConn dcon, 
			String[] methAns, 
			String methodNm) throws Exception {
		makeAnns(jw, methAns);
		String retTp = "PersistenceExceptionTranslationPostProcessor";
		jw.beginMethod(retTp, methodNm, getMods());
		for (String stm: methodFour){
			jw.emitStatement(stm);
		}
		jw.endMethod();
	}
		
	private static void createFn5(
			JavaWriter jw, 
			DataBaseConn dcon) throws Exception {
		String methNm = "additionalProperties";
		String retTp = "Properties";
		Set<Modifier> mods = new HashSet<Modifier>();
		mods.add(Modifier.PRIVATE);
		// modifiers CAN'T be empty
		
		jw.beginMethod(
				retTp, 
				methNm, 
				mods);
		for (String stm : methodFive){
			try{
				jw.emitStatement(stm);
			}catch (Throwable e){
				jw.emitStatement("Messy String");
			}
		}
		jw.endMethod();
	}


	private static String quote(String s){
		return "\"" + s + "\"";
	}
	
	private static Set<Modifier> getMods(){
		Set<Modifier> mods = new HashSet<Modifier>();
		mods.add(Modifier.PUBLIC);
		return mods;
	}
	
	
}
