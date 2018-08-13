// SimpleWorker

exports._newWorker = function(url) {
  return new Worker(url);
};

exports._request = function(args, callback, worker) {
  worker.onmessage = function(e) {
    callback(e.data)();
  };

  worker.postMessage(args);

  return {};
};

exports._response = function(callback) {
  onmessage = function(e) {
    postMessage(callback(e.data)());
  };
}
