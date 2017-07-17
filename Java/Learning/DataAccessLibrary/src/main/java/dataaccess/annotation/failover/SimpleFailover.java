package dataaccess.annotation.failover;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.annotation.ElementType;

/**
 * Rules (self explanatory):
 *  1) No empty strings
 *  2) The Uid references are vaild
 * 
 * @author divesh
 *
 */


@Retention(RetentionPolicy.CLASS)
@Target(ElementType.TYPE)
public @interface SimpleFailover {
	String defaultDbUid();
	String failoverDbUid();

}
