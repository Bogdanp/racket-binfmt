#lang binfmt

Size = i32be;

RequestHeader = APIKey APIVersion CorrelationID ClientID;
APIKey = i16be;
APIVersion = i16be;
CorrelationID = i32be;
ClientID = String;

ResponseHeader = CorrelationID;

MetadataRequest = ArrayLen Topic{ArrayLen_1};
Topic = String;

MetadataResponse = ArrayLen Broker{ArrayLen_1} ControllerID ArrayLen TopicMetadata{ArrayLen_2};

Broker = NodeID Host Port;
NodeID = i32be;
Host = String;
Port = i32be;
Rack = String;

ControllerID = i32be;

TopicMetadata = TopicErrorCode Topic IsInternal ArrayLen PartitionMetadata{ArrayLen_1};
TopicErrorCode = i16be;
IsInternal = u8;

PartitionMetadata = PartitionErrorCode PartitionID Leader ArrayLen Replica{ArrayLen_1} ArrayLen ISR{ArrayLen_2};
PartitionErrorCode = i16be;
PartitionID = i32be;
Leader = i32be;
Replica = i32be;
ISR = i32be;

ArrayLen = i32be;

Bytes = NullBytes | BytesLength BytesData{BytesLength_1};
NullBytes = 0xFF 0xFF 0xFF 0xFF;
BytesLength = i32be;
BytesData = u8;

String = NullString | StringLength StringData{StringLength_1};
NullString = 0xFF 0xFF;
StringLength = i16be;
StringData = u8;
