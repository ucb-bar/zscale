organization := "edu.berkeley.cs"

version := "1.0"

name := "zscale"

scalaVersion := "2.11.6"

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
libraryDependencies ++= (Seq("chisel", "junctions", "uncore", "rocket").map {
  dep: String => sys.props.get(dep + "Version") map { "edu.berkeley.cs" %% dep % _ }}).flatten
