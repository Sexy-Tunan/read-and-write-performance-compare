对process,ets dets, mnesia(ram copies),mnesia(disc_copies),mnesia(disc only_copies)
六种不同的数据存储方式进行读写性能比较。要求:

1. 提前存储了一万条记录(record，第一个字段就是 key)，
2. 数据记录都采用 set 方式存储。
3. 同样数据量和读写请求之下，测试分别读、写一万次，需要消耗的时间。(注意:总共有 6*2=12 个时间结果)。


结论: 