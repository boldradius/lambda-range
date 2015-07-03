# lambda-range
Fast and simple spatial search library with a purely functional interface. At this point it only supports two-dimentinal
circular queries on point data. The implementation uses a quad-tree immutable data structure.

## Usage

Add a resolver and the library dependency to your sbt project (this resolver type is built-in to sbt starting from 0.13.6):

    resolvers += Resolver.bintrayRepo("boldradiussolutions", "maven")

    libraryDependencies += "com.boldradius" %% "lambda-range" % "0.1.0"

Then import the package, instantiate a QuadTree with a Seq of elements and a 2D range, and perform queries like this:

    import com.boldradius.lambdarange._

    val elements = List(
      Element("Here", 0.5f, 0.2f),
      Element("There", 0.1f, 0.1f))
    val tree = QuadTree(elements, 0, 1, 0, 1)
    tree.inRadius(0.2f, (0.4f, 0.2f))
    
    // Returns Array(Element(Here,0.5,0.2))
