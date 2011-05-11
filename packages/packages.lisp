(defpackage #:srt-kd
  (:nicknames :kd)
  (:use :cl
        :iterate
        :parse-number
        :alexandria
        :cl-ppcre
        :metabang-bind
        :cffi
        :gl)
  (:shadowing-import-from :iterate #:finish)
  (:shadowing-import-from :alexandria #:rotate))
