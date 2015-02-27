// Provide a managed dependency on chisel if -DrocketVersion="" is
// supplied on the command line.

val rocketVersion_r = System.getProperty("rocketVersion", "None")

// _r a temporary fix until sbt 13.6 https://github.com/sbt/sbt/issues/1465

libraryDependencies ++= ( if (rocketVersion_r != "None" ) (
    "edu.berkeley.cs" %% "rocket" % rocketVersion_r
) :: Nil; else Nil)
