package com.phasmidsoftware.tableparser.kafka.cats

import java.nio.charset.StandardCharsets
import org.apache.kafka.clients.consumer.{ConsumerRecord, MockConsumer, OffsetAndMetadata, OffsetCommitCallback}
import org.apache.kafka.common.TopicPartition

/**
 * A Kafka MockConsumer for the tests of fromKafka: no broker is needed.
 */
object MockKafka:

  /**
   * A MockConsumer which remembers each offset that is committed
   * (MockConsumer's own committed method can't be called once the consumer has been closed, as it is when a stream ends).
   */
  class RecordingConsumer extends MockConsumer[Array[Byte], Array[Byte]]("earliest"):
    @volatile var committedOffsets: List[Long] = Nil

    override def commitAsync(offsets: java.util.Map[TopicPartition, OffsetAndMetadata], callback: OffsetCommitCallback): Unit =
      offsets.values().forEach(o => committedOffsets = committedOffsets :+ o.offset())
      super.commitAsync(offsets, callback)

    override def commitSync(offsets: java.util.Map[TopicPartition, OffsetAndMetadata]): Unit =
      offsets.values().forEach(o => committedOffsets = committedOffsets :+ o.offset())
      super.commitSync(offsets)

  /**
   * A RecordingConsumer of a topic with one partition: its first poll assigns the partition and delivers the first batch
   * of messages; each later poll, the next batch.
   *
   * @param topic   the topic.
   * @param batches the batches of messages, each a header (the tableparser.columns header; none if empty) and a value
   *                (the key of each message is its offset).
   * @return a RecordingConsumer.
   */
  def consumer(topic: String, batches: Seq[Seq[(String, String)]]): RecordingConsumer =
    val mock = new RecordingConsumer
    val partition = new TopicPartition(topic, 0)
    val offsets = batches.scanLeft(0L)(_ + _.size)
    for ((batch, first), i) <- batches.zip(offsets).zipWithIndex do
      mock.schedulePollTask: () =>
        if i == 0 then
          mock.rebalance(java.util.List.of(partition))
          mock.updateBeginningOffsets(java.util.Map.of(partition, java.lang.Long.valueOf(0L)))
        for ((header, value), j) <- batch.zipWithIndex do
          val record = new ConsumerRecord[Array[Byte], Array[Byte]](topic, 0, first + j, (first + j).toString.getBytes(StandardCharsets.UTF_8), value.getBytes(StandardCharsets.UTF_8))
          if header.nonEmpty then record.headers().add(IOKafka.columnsHeader, header.getBytes(StandardCharsets.UTF_8)): Unit
          mock.addRecord(record)
    mock
