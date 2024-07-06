import Foundation

try ProcessInfo.processInfo.arguments[1...]
  .filter(FileManager.default.fileExists)
  .map(URL.init(fileURLWithPath:))
  .forEach {
    try FileManager.default.trashItem(at: $0, resultingItemURL: nil)
  }
