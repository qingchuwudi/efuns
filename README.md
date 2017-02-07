# efuns

Erlang项目中常用的功能方法。

包括：

- 时间处理

	- [x] utc
	- [x] iso
	- [x] 时间的可视化处理
	- [x] 其它

- IP处理

	- [x] 在socket中提取ip
	- [x] ipv4、ipv6在元组、字符串、二进制之间的转换
	- [x] 其它

- 数据

	- [x] 格式转换
	- [x] 大小端
	- [x] 16进制转换
	- [x] 其它

- 全局进程相关函数

## 使用

- 下载

    ```bash
    $ git clone https://github.com/qingchuwudi/efuns.git
    ```
- 编译

    ```bash
    $ cd efuns
    $ make
    ```
- 使用

	启动
    ```bash
    $ make console
    ```

    调用
    ```erlang
    1> etime:local2utc().
    "2017-02-07T12:12:00.578806Z"
    ```