package dataaccess.annotation.database;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 *	Rules 
 *	//TODO this soon. 
 * 
 * @author divesh
 *
 */


@Retention(RetentionPolicy.CLASS)
@Target(ElementType.TYPE)
public @interface DataAccessConfig {
	DataBaseConn primaryDb();
	DataBaseConn[] secondaryDbs();
}
