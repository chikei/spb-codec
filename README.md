# spb codec

![build](https://github.com/chikei/spb-codec/workflows/Tests/badge.svg)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.chikei/spb-codec-core_2.13)](https://maven-badges.herokuapp.com/maven-central/io.github.chikei/spb-codec-core_2.13)

spb codec is a [Protobuf 3](https://developers.google.com/protocol-buffers/docs/proto3) codec library for Scala.

spb codec is based on now archived [protoless](https://github.com/julien-lafont/protoless) project and modified for my requirement.

## QuickStart

spb codec is only built for `scala 2.13` now.

```scala
libraryDependencies ++= Seq(
  "io.github.chikei" %% "spb-codec-magnolia" % "0.1.0",
  "io.protoless" %% "protoless-generic" % "0.0.7"
)
```

```scala
import io.github.chikei.spbcodec.magnolia.decoding.auto._
import io.github.chikei.spbcodec.magnolia.encoding.auto._

case class Person(firstname: String, lastname: String, age: Option[Int], locations: Seq[String])

val p = Person("John", "Doe", Some(28), Seq("Paris", "London", "New York"))
val byte = Encoder[Person].encodeAsBytes(p) // or p.asProtobufBytes
val decoded = Decoder[Person].decode(bytes)
```

## Why?

[ScalaPB](https://github.com/scalapb/ScalaPB) is a decent scala library for protobuf, but it requires proto definition files to work with.
For my case, what I need is something that can automatcally derive most case from scala case class model and I don't really care long term stability of binary format.

## Contributing

The protoless project welcomes contributions from **anybody wishing to participate**. All code or documentation that is provided must be licensed with the same license that Protoless is licensed with (Apache 2.0, see LICENSE file).

Feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Pull requests are also gladly accepted. You can also just enter in the gitter channel to talk with us.

## License

Code is provided under the Apache 2.0 license available at http://opensource.org/licenses/Apache-2.0, as well as in the LICENSE file.
