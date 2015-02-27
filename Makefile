SBT := sbt -DchiselVersion=2.3-SNAPSHOT -DuncoreVersion=2.0 -DrocketVersion=1.2

compile:
	$(SBT) compile
