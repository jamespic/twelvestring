package com.stackexchange.codegolf.twelvestring

import edu.emory.mathcs.jtransforms.dht.DoubleDHT_1D
import java.lang.Math._
import scala.collection.{mutable, breakOut}
import java.io._
import nayuki.arithcode._
import scala.Some
import java.math.BigInteger
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D

/**
 * Created by hisg085 on 28/04/2014.
 */
object TwelveString {
  val SilentNote = -1
  val NoChangeCode = 0
  val SilenceCode = 12
  val CjkSize = 20992
  val CjkStart = 0x4E00

  val blocksPerSecond = 14
  val samplesPerSecond = 44100
  val blockSize = samplesPerSecond / blocksPerSecond
  val concertA = 440
  val concertAInBlock = concertA.toDouble / blocksPerSecond
  val concertATimeDomain = samplesPerSecond.toDouble / concertA
  val lowerCutoff = 50
  val lowerCutoffInBlock = lowerCutoff.toDouble / blocksPerSecond
  val upperCutoff = 11025
  val upperCutoffInBlock = upperCutoff.toDouble / blocksPerSecond
  val threshold = 0.02
  val noiseThreshold = 2.5
  val noteWidth = 0.375
  val tukeyAlpha = 0.3

  def note(f: Double) = {
    val roughNote = (log(f) - log(concertA)) / log(2) * 12
    val exactNote = round(roughNote)
    if (abs(roughNote - exactNote) < noteWidth) mod12(exactNote.toInt) else SilentNote
  }

  val notes = for (n <- 0 until blockSize) yield note(n * blocksPerSecond)

  val dhtTransformer = new DoubleDHT_1D(blockSize)

  def weighting(f: Double) = if ((f <= upperCutoff) && (f >= lowerCutoff)) aWeighting(f) else 0.0

  val weightings = for (n <- 0 until blockSize) yield weighting(n * blocksPerSecond)

  val soundBlocks = {
    for (i <- 0 until 12) yield {
      val centreFrequency = concertAInBlock * 2.0 ~^ (i / 12.0)
      val frequencies = for (i <- -10 to 10; f = centreFrequency * 2 ~^ i; if lowerCutoffInBlock <= f && f <= upperCutoffInBlock) yield round(f).toInt
      val dht: Array[Double] = (0 until blockSize).map(f => if (frequencies contains f) 1.0 else 0.0)(breakOut)
      dhtTransformer.inverse(dht, false)
      dht
    }
  }

  val emptyBlock = Array.fill[Short](blockSize)(0)

  val soundBlocks16Bit = {
    val max = soundBlocks.map(_.max).max
    val factor = 0.5 * Short.MaxValue / max
    soundBlocks.map(block => block.map(v => (v * factor).toShort))
  }

  val frequencyTable = new nayuki.arithcode.SimpleFrequencyTable(
    Array(10, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2) // Initial heuristic - adaptive coding will take over
  )

  val lengthTable = new nayuki.arithcode.SimpleFrequencyTable({
    val array = Array.fill(80 * 60 * blocksPerSecond)(1) // Allow any length up to 80 minutes
    for (i <- 0 to 5 * 60 * blocksPerSecond) array(i) = 20 // Lengths under 5 minutes need less entropy
    array(60 * blocksPerSecond) = 1000 // Clips of exactly 60 seconds need less entropy still (yes, I know it's cheating)
    array
  })

  implicit class Powerable(val x: Double) extends AnyVal {
    def ~^(p: Double) = pow(x, p)
  }

  def aWeighting(f: Double) = {
    12200 ~^ 2 * f ~^ 4 / ((f ~^ 2 + 20.6 ~^ 2) * sqrt((f ~^ 2 + 107.7 ~^ 2) * (f ~^ 2 + 737.9 ~^ 2)) * (f ~^ 2 + 12200 ~^ 2))
  }

  def mod12(i: Int) = {
    val x = i % 12
    if (x >= 0) x else x + 12
  }

  def window(i: Int) = i match {
    case n if i <= tukeyAlpha * (blockSize - 1) / 2 => (1 + cos(PI * (2 * n / (tukeyAlpha * (blockSize - 1)) - 1))) / 2
    case n if i >= (blockSize - 1) * (1 - tukeyAlpha / 2) => (1 + cos(PI * (2 * n / (tukeyAlpha * (blockSize - 1)) - 2 / tukeyAlpha + 1))) / 2
    case _ => 1.0
  }

  def makeBlocks(input: Array[Double], debugIndex: Option[Int] = None) = {
    val blocks = for (block <- input.grouped(blockSize) if block.length == blockSize) yield {
      //val data = block.map(_.toDouble)
      val data = block.zipWithIndex map {case (x, i) => x * window(i)}
      dhtTransformer.forward(data)
      val frequencies = new Array[Double](12)
      for (f <- 1 to blockSize / 2) {
        val x = sqrt(data(f) ~^ 2 + data(blockSize - f) ~^ 2)
        val note = notes(f)
        if (note != SilentNote) {
          val impact = abs(x * weightings(f))
          if (Some(note) == debugIndex) println(s"Adding $impact (freq $f, volume $x) to note $note")
          frequencies(note) = frequencies(note) max impact
        }
      }
      frequencies.toIndexedSeq
    }
    blocks.toIndexedSeq
  }
  
  def makeBlocks16Bit(input: Array[Short]) = makeBlocks(input map (_.toDouble))

  def simpleNoteMaker(blocks: IndexedSeq[IndexedSeq[Double]]) = {
    val maxVol = blocks.flatten.max
    for (block <- blocks) yield {
      val (vol, note) = block.zipWithIndex.maxBy(_._1)
      if (vol > maxVol * threshold) note else SilentNote
    }
  }

  def tweakableNoteMaker(blocks: IndexedSeq[IndexedSeq[Double]], quality: Double) = {
    val maxVol = blocks.flatten.max
    var lastNote = 0
    for (block <- blocks) yield {
      val (vol, note) = block.zipWithIndex.maxBy(_._1)
      val minVol = block.min
      //if (vol < maxVol * threshold) SilentNote
      if (vol < minVol * noiseThreshold) SilentNote
      else {
        if (block(lastNote) > quality * vol) lastNote
        else {
          try note
          finally lastNote = note
        }
      }
    }
  }
  def noteEntropiser(notes: Seq[Int]) = {
    var currentNote = 0
    var silent = false
    for (note <- notes) yield {
      if (note == SilentNote) {
          if (silent) {
            NoChangeCode
          } else {
            silent = true
            SilenceCode
          }
      } else if (silent && (currentNote == note)) {
        silent = false
        SilenceCode
      } else {
        try mod12(note - currentNote)
        finally currentNote = note
      }
    }
  }

  def noteDeentropiser(codes: Seq[Int]) = {
    var currentNote = 0
    var silent = false
    for (code <- codes) yield {
      if (code == SilenceCode) {
        if (silent) {
          silent = false
          currentNote
        } else {
          silent = true
          SilentNote
        }
      } else {
        if (silent & (code == NoChangeCode)) {
          SilentNote
        } else {
          currentNote = mod12(currentNote + code)
          currentNote
        }
      }
    }
  }
  
  def outputFromNoteSeq(noteSeq: Seq[Int]) = {
    val outputMaker = new mutable.ArrayBuilder.ofShort
    for (note <- noteSeq) {
      outputMaker ++= (if (note < 0) emptyBlock else soundBlocks16Bit(note))
    }
    outputMaker.result()
  }

  def write(data: Array[Short], file: String) = {
    val output = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
    try {
      for (s <- data) output.writeShort(s)
    } finally output.close()
  }

  def read(file: String) = {
    val input = new DataInputStream(new BufferedInputStream(new FileInputStream(file)))
    val blockBuilder = new mutable.ArrayBuilder.ofShort
    try {
      while (true) {
        blockBuilder += input.readShort
      }
    } catch {
      case _: EOFException => // This is normal
    } finally {
      input.close()
    }
    blockBuilder.result()
  }

  def simplePort(input: String, output: String) = {
    val data = read(input)
    val blocks = makeBlocks16Bit(data)
    val blockSeq = simpleNoteMaker(blocks)
    val o = outputFromNoteSeq(blockSeq)
    write(o, output)
  }

  def tweakablePort(input: String, output: String, quality: Double) = {
    val data = read(input)
    val blocks = makeBlocks16Bit(data)
    val blockSeq = tweakableNoteMaker(blocks, quality)
    val o = outputFromNoteSeq(blockSeq)
    write(o, output)
  }

  def encode(input: String, quality: Double) = {
    val table = new SimpleFrequencyTable(frequencyTable) // Allow updating, for adaptive compression
    val data = read(input)
    val blocks = makeBlocks16Bit(data)
    val noteSeq = tweakableNoteMaker(blocks, quality)
    val entropised = noteEntropiser(noteSeq)
    val outputStream = new ByteArrayOutputStream
    val byteOutput = new BitOutputStream(outputStream)
    val encoder = new ArithmeticEncoder(byteOutput)
    encoder.write(lengthTable, entropised.length)
    for (code <- entropised) {
      encoder.write(table, code)
      table.increment(code)
    }
    encoder.finish()
    byteOutput.close()
    outputStream.toByteArray
  }

  def decode(input: Array[Byte], output: String) = {
    val table = new SimpleFrequencyTable(frequencyTable) // Allow updating, for adaptive compression
    val inputStream = new ByteArrayInputStream(input)
    val bitInput = new BitInputStream(inputStream)
    val decoder = new ArithmeticDecoder(bitInput)
    val decodedBuilder = IndexedSeq.newBuilder[Int]
    val length = decoder.read(lengthTable)
    for (i <- 0 until length) {
      val code = decoder.read(table)
      table.increment(code)
      decodedBuilder += code
    }
    val decoded = decodedBuilder.result()
    val deentropised = noteDeentropiser(decoded)
    val o = outputFromNoteSeq(deentropised)
    write(o, output)
  }

  def bytesAtQuality(input: String, quality: Double) = {
    encode(input, quality).length
  }

  def encodeCJK(data: Array[Byte]) = {
    var dataAsInt = new BigInt(new BigInteger((1.toByte +: data))) // Ensure first byte is 1 - avoids all kinds of hassle
    val encodingBuilder = new StringBuilder
    while (dataAsInt > 0) {
      val remainder = dataAsInt % CjkSize
      encodingBuilder += (CjkStart.intValue + remainder.intValue).toChar
      dataAsInt = dataAsInt / CjkSize
    }
    encodingBuilder.result()
  }

  def decodeCJK(data: String) = {
    var dataAsInt: BigInt = 0
    for (c <- data.reverse) {
      dataAsInt *= CjkSize
      dataAsInt += (c - CjkStart)
    }
    dataAsInt.toByteArray.drop(1)
  }

  def encodeFileToString(input: String, quality: Double) = {
    val data = encode(input, quality)
    encodeCJK(data)
  }

  def decodeString(compressed: String, output: String) = {
    val data = decodeCJK(compressed)
    decode(data, output)
  }

  def extractTable(input: String, quality: Double) = {
    val data = read(input)
    val blocks = makeBlocks16Bit(data)
    val noteSeq = tweakableNoteMaker(blocks, quality)
    val entropised = noteEntropiser(noteSeq)
    val table = new nayuki.arithcode.SimpleFrequencyTable(new Array[Int](13))
    for (code <- entropised) table.increment(code)
    table
  }

  def encodeToSize(input: String, limit: Int): String = {
    for (quality <- 1.0 to 0.05 by -0.05) {
      val encoded = encodeFileToString(input, quality)
      if (encoded.size <= limit) return encoded
    }
    throw new Exception("Impossible!")
  }

  def main(args: Array[String]) = {
    args(0) match {
      case "encode" =>
        val encoded = encodeToSize(args(1), 140)
        val writer = new OutputStreamWriter(new FileOutputStream(args(2)), "UTF-8")
        try {
          writer.write(encoded)
        } finally writer.close()
        println(s"Encoded ${args(1)} as $encoded")
      case "decode" =>
        val reader = scala.io.Source.fromFile(args(1), "UTF-8")
        val encoded = reader.getLines().mkString("")
        decodeString(encoded, args(2))
        println(s"Saved $encoded to ${args(2)}")
    }
  }
}
