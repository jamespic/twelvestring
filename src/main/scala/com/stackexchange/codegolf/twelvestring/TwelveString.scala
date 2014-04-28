package com.stackexchange.codegolf.twelvestring

import edu.emory.mathcs.jtransforms.dht.DoubleDHT_1D
import java.lang.Math._
import scala.collection.{mutable, breakOut}
import java.io._
import scala.Some
import nayuki.arithcode._
import edu.emory.mathcs.jtransforms.dct.DoubleDCT_1D
import scala.Some
import java.math.BigInteger

/**
 * Created by hisg085 on 28/04/2014.
 */
object TwelveString {
  val SilentNote = -1
  val NoChangeCode = 0
  val SilenceCode = 12
  val EOFCode = 13
  val CjkSize = 20992
  val CjkStart = 0x4E00

  val blocksPerSecond = 14
  val samplesPerSecond = 44100
  val blockSize = samplesPerSecond / blocksPerSecond
  val concertA = 440
  val concertAInBlock = concertA / blocksPerSecond
  val lowerCutoff = 50
  val lowerCutoffInBlock = lowerCutoff.toDouble / blocksPerSecond
  val upperCutoff = 11025
  val upperCutoffInBlock = upperCutoff.toDouble / blocksPerSecond
  val threshold = 0.02
  val noiseThreshold = 3.5
  val notes = {
    for (n <- 0 until blockSize) yield {
      mod12(round((log(n) - log(concertAInBlock)) / log(2) * 12).toInt)
    }
  }
  val transformer = new DoubleDHT_1D(blockSize)

  val weightings = {
    for (n <- 0 until blockSize) yield {
      val freq = n * blocksPerSecond
      if ((freq <= upperCutoff) && (freq >= lowerCutoff)) aWeighting(freq) / freq else 0
    }
  }

  val soundBlocks = {
    for (i <- 0 until 12) yield {
      val centreFrequency = concertAInBlock * 2.0 ~^ (i / 12.0)
      val frequencies = for (i <- -10 to 10; f = centreFrequency * 2 ~^ i; if lowerCutoffInBlock <= f && f <= upperCutoffInBlock) yield round(f).toInt
      val dht: Array[Double] = (0 until blockSize).map(f => if (frequencies contains f) 1.0 else 0.0)(breakOut)
      transformer.inverse(dht, false)
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
    Array(10, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1) // Initial heuristic - adaptive coding will take over
  )

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

  def makeBlocks(input: Array[Double], debugIndex: Option[Int] = None) = {
    val blocks = for (block <- input.grouped(blockSize) if block.length == blockSize) yield {
      val data = block.map(_.toDouble)
      transformer.forward(data)
      val frequencies = new Array[Double](12)
      for ((x, i) <- data.zipWithIndex) {
        val note = notes(i)
        val impact = abs(x * weightings(i))
        if (Some(note) == debugIndex) println(s"Adding $impact (freq $i, volume $x) to note $note")
        frequencies(note) += impact
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
      if (vol / minVol < noiseThreshold) SilentNote
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
    } finally output.close
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
    for (code <- entropised) {
      encoder.write(table, code)
      table.increment(code)
    }
    encoder.write(table, EOFCode)
    table.increment(EOFCode)
    encoder.finish()
    outputStream.toByteArray
  }

  def decode(input: Array[Byte], output: String) = {
    val table = new SimpleFrequencyTable(frequencyTable) // Allow updating, for adaptive compression
    val inputStream = new ByteArrayInputStream(input)
    val bitInput = new BitInputStream(inputStream)
    val decoder = new ArithmeticDecoder(bitInput)
    var code = 0
    val decodedBuilder = IndexedSeq.newBuilder[Int]
    def more = {
      code = decoder.read(table)
      table.increment(code)
      code != EOFCode // Why isn't this working???
    }
    while (more) {
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
    val table = new nayuki.arithcode.SimpleFrequencyTable(new Array[Int](14))
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
}
