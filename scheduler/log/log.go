package log

import (
	serr "github.com/koki/structurederrors"
	"github.com/sirupsen/logrus"
)

var Default = Logger{}

type Logger struct {
	Context map[string]interface{}
}

func (l *Logger) StartLog() Log {
	log := StartLog()
	for key, val := range l.Context {
		log[key] = val
	}

	return log
}

// Log is an unstructured log object used to generate JSON-formatted logs.
// Information is added to the Log over the course of its lifetime until
// its Write method is called (usually by a "defer" block).
type Log map[string]interface{}

// StartLog starts a new Log entry.
func StartLog() Log {
	return Log(map[string]interface{}{})
}

// Write actually commits the log entry (usually in a "defer" block).
func (l Log) Write() {
	hasError := false
	if e, ok := l["error"]; ok {
		hasError = true
		if e, ok := e.(error); ok {
			l["error"] = e.Error()
		}
	}

	if hasError {
		logrus.WithField("log", &l).Error()
	} else {
		logrus.WithField("log", &l).Info()
	}
}

func (l *Logger) FatalErr(err error, contextFormat string, contextArgs ...interface{}) {
	log := StartLog()
	log["error"] = serr.ContextualizeErrorf(err, contextFormat, contextArgs...).Error()
	logrus.WithField("log", &log).Fatal()
}

func (l *Logger) Err(err error, contextFormat string, contextArgs ...interface{}) {
	log := StartLog()
	log["error"] = serr.ContextualizeErrorf(err, contextFormat, contextArgs...).Error()
	log.Write()
}

func (l *Logger) PlainErr(err error) {
	log := StartLog()
	log["error"] = err
	log.Write()
}

func (l *Logger) Info(messageFormat string, args ...interface{}) {
	logrus.Infof(messageFormat, args...)
}
