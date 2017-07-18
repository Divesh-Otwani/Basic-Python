package dataaccess.processor;



import java.util.LinkedHashSet;
import java.util.Set;
import java.util.HashMap;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Filer;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;

import javax.lang.model.util.Elements;
//import javax.lang.model.util.Types;
//import javax.tools.Diagnostic;
import javax.tools.Diagnostic.Kind;


import dataaccess.annotation.database.DataAccessConfig;
import dataaccess.annotation.database.DataBaseConn;
import dataaccess.annotation.failover.SimpleFailover;
import dataaccess.processor.MakeConfigFiles;



public class DataAccessProcessor extends AbstractProcessor {

	// private Types typeUtils;
	private Elements elementUtils;
	private Filer filer;
	private Messager messager;
	
	

	@Override
	public synchronized void init(ProcessingEnvironment processingEnv) {
		super.init(processingEnv);
		// typeUtils = processingEnv.getTypeUtils();
		elementUtils = processingEnv.getElementUtils();
		filer = processingEnv.getFiler();
		messager = processingEnv.getMessager();
	}

	@Override
	public Set<String> getSupportedAnnotationTypes() {
		Set<String> anns = new LinkedHashSet<String>();
		anns.add(SimpleFailover.class.getCanonicalName());
		anns.add(DataAccessConfig.class.getCanonicalName());
		anns.add(DataBaseConn.class.getCanonicalName());
		return anns;
	}

	@Override
	public SourceVersion getSupportedSourceVersion() {
		return SourceVersion.latestSupported();
	}

	@Override
	public boolean process(Set<? extends TypeElement> annotations, 
			RoundEnvironment roundEnv) {
		try{
			return realProcess(annotations, roundEnv);
		}catch (Throwable e){
			String err = 
					"Msg: " + 
					e.getMessage() + 
					", toStr: " + 
					e.toString();
			error(err);
			return false;
		}
	}
	
	private boolean realProcess(
			Set<? extends TypeElement> annotations, 
			RoundEnvironment roundEnv) throws Exception {
		
		// 1. check @dbcons is nested
		boolean noDbConns = 
				roundEnv
				.getElementsAnnotatedWith(DataBaseConn.class)
				.isEmpty();
		String dbconsEr = "Can't annotate objects with DataBaseConn";
		myAssert(noDbConns, dbconsEr);
		
		
		// 2. check and generate config files 
		// w/ DataAccessConfig annotation
		Element dbConf = 
				roundEnv
				.getElementsAnnotatedWith(DataAccessConfig.class)
				.iterator()
				.next();
		
		boolean oneConfig = roundEnv
				.getElementsAnnotatedWith(DataAccessConfig.class)
				.size() == 1;
		String noOneConf = "Need exactly one DataAccessConfig annotation";
		myAssert(oneConfig, noOneConf);
		
		DataAccessConfig dataAccessConfig = dbConf.getAnnotation(DataAccessConfig.class);
		checkDataAccessRules(dbConf);
		
		if (dbConf.getKind() != ElementKind.CLASS){ 
			error(" dataAccessConf annotation should be on a class.");
			return false;
		}
		
		TypeElement configClass = (TypeElement) dbConf;
		String messPkgNm = 
				elementUtils
				.getPackageOf(configClass)
				.toString();
		String pkgNm = cleanPkgName(messPkgNm);
		String classNm = configClass.getSimpleName().toString();
		HashMap<String, DataBaseConn> dbUids =
				MakeConfigFiles.createConfigFiles(
						dataAccessConfig, 
						pkgNm, 
						classNm,
						filer);
		
		// 3. Set up failovers.
		setUpFailovers(roundEnv, dbUids);
		return true;
	}
	

	private void error(String msg) {
		messager.printMessage(Kind.ERROR, msg);
	}
	
	private void myAssert(Boolean b, String msg) throws Exception {
		// want bool to be true.
		if (!b){
			error(msg);
			throw new Exception("assertion failed: " + msg);
		}
	}

	private String cleanPkgName(String s) {
		String[] sp = s.split("\\s+");
		return sp[1];
	}
	
	private void checkDataAccessRules(Element annotatedCls)
	throws Exception {
		
	}
	
	private void setUpFailovers(
			RoundEnvironment roundEnv,
			HashMap<String, DataBaseConn> uidMap) throws Exception{
		
		for( Element annInterface : 
			roundEnv.getElementsAnnotatedWith(SimpleFailover.class)){
			// 1. check valid
			checkValidSimpleFailover(annInterface);
			
			// 2. create repos
			TypeElement annInt = (TypeElement) annInterface;
			SimpleFailover ann = 
					annInt.getAnnotation(SimpleFailover.class);
			String messPkgNm = elementUtils
					.getPackageOf(annInt)
					.toString();
			String pkgNm = cleanPkgName(messPkgNm);
			String classNm = annInt.getSimpleName().toString();
			String repoOneUid = ann.defaultDbUid();
			String repoTwoUid = ann.failoverDbUid();
			
			String repoOnePkgNm = 
					classNm + uidMap.get(repoOneUid).repoPkgNm();
			String repoTwoPkgNm = 
					classNm + uidMap.get(repoTwoUid).repoPkgNm();
			String repoOneNm = classNm + repoOneUid;
			String repoTwoNm = classNm + repoTwoUid;
			String repoOneFullNm = 
					repoOnePkgNm + "." + repoOneNm;
			String repoTwoFullNm = 
					repoTwoPkgNm + "." + repoTwoNm;
			
			createRepo(repoOnePkgNm, repoOneNm, annInt);
			createRepo(repoTwoPkgNm, repoTwoNm, annInt);
			
			// 3. create overlapping failover repo
			createSimpleFailover(repoOneFullNm, repoTwoFullNm, annInt, pkgNm);
		}
		
	}

	private void createSimpleFailover(String repoOneFullNm, 
			String repoTwoFullNm, 
			TypeElement annInt,
			String annIntPkgNm) throws Exception {
		
		
	}

	private void createRepo(
			String repoOnePkgNm,
			String repoOneNm,
			TypeElement annInt) throws Exception {
		
	}

	private void checkValidSimpleFailover(Element e) 
	throws Exception {
		
	}
}













