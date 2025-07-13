Map<String, Function> _structParsers = {};

Future<void> main() async {
  class Sensor {
    String id;
    double temperature;
    Sensor({required this.id, required this.temperature});
    factory Sensor.fromJson(Map<String,dynamic> m) {
      return Sensor(id: m['id'] as String, temperature: m['temperature'] as double);
    }
  }
  
  var _SensorStream = _Stream<Sensor>('Sensor');
  void _handler_0(Sensor ev) {
    var s = ev;
    print([s.id.toString(), s.temperature.toString()].join(' '));
  }
  _SensorStream.register(_handler_0);
  _SensorStream.append(Sensor({id: "sensor-1", temperature: 22.5}));
  await _waitAll();
}

class _Stream<T> {
    String name;
    List<void Function(T)> handlers = [];
    _Stream(this.name);
    Future<T> append(T data) {
        var tasks = <Future<dynamic>>[];
        for (var h in List.from(handlers)) {
            var res = h(data);
            if (res is Future) tasks.add(res);
        }
        var f = Future.wait(tasks).then((_) => data);
        _pending.add(f);
        return f;
    }
    void register(void Function(T) handler) { handlers.add(handler); }
}

List<Future<dynamic>> _pending = [];
Future<void> _waitAll() async {
    await Future.wait(_pending);
}
