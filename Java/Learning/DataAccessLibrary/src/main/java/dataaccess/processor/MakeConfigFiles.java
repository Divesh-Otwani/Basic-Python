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
					pkgNm +
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
		Set<Modifier> mods = new HashSet<Modifier>();
		mods.add(Modifier.PUBLIC);
		jw.emitImports(imports);
		jw.emitAnnotation("Configuration");
		HashMap<String, String> jpaArgs =
				new HashMap<String, String>();
		jpaArgs.put("basePackages", emfNm);
		jpaArgs.put("entityManagerFactoryRef", tmrNm);
		jw.emitAnnotation("EnableJpaRepositories", jpaArgs);
		jw.beginType(clNm, "class", mods);
		
		String[] methAns = 
				(isPrimary) ? 
						new String[] {"Bean", "Primary"} : 
						new String[] {"Bean"};
		
		createFn1(jw, dcon, methAns, emfNm);
		createFn2(jw, dcon, methAns, dtaNm);
		createFn3(jw, dcon, methAns, tmrNm);
		createFn4(jw, dcon, methAns, extNm);
		
		jw.endType();
		jw.close();
	
	}

	private static void createFn4(
			JavaWriter jw, 
			DataBaseConn dcon, 
			String[] methAns, 
			String methodNm) {
		
	}

	private static void createFn3(
			JavaWriter jw, 
			DataBaseConn dcon, 
			String[] methAns, 
			String methodNm) {
		// TODO Auto-generated method stub
		
	}

	private static void createFn2(
			JavaWriter jw, 
			DataBaseConn dcon, 
			String[] methAns, 
			String methodNm) {
		// TODO Auto-generated method stub
		
	}

	private static void createFn1(
			JavaWriter jw, 
			DataBaseConn dcon, 
			String[] methAnsn, 
			String methodNm) {
		// TODO Auto-generated method stub
		
	}
	
	
}
