package dataaccess.annotation.database;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Rules
 * 1) No empty strings
 * 2) This is used inside another annotation,
 * and not anywhere else
 * 3) The uniqueId begins with a capital letter
 * and is a valid class name
 * 
 * @author divesh
 *
 */


@Retention(RetentionPolicy.CLASS)
public @interface DataBaseConn {
	String url();
	String username();
	String password();
	String dbDriverNm();
	String repoPkgNm();
	String domainPkgNm();
	String uniqueId();
}
