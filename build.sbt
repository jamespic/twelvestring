name := "twelvestring"

version := "1.0"

libraryDependencies ++= Seq(
  "com.github.rwl" % "jtransforms" % "2.4.0",
  "nayuki" % "arithcode" % "1.0"
)

resolvers += "Local Maven Repository" at "file:///"+Path.userHome+"/.m2/repository"