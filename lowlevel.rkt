#lang racket
(require (for-syntax racket/syntax)
         srfi/26 ffi/unsafe file/resource)

(define _jboolean (make-ctype _uint8
                              (lambda (s) (if s 1 0))
                              (lambda (c) (not (zero? c)))))
(define _jbyte _sint8)
(define _jchar (make-ctype _uint16 char->integer integer->char))
(define _jint _sint32)
(define _jlong _sint64)
(define _jshort _sint16)
(define _jfloat _float)
(define _jdouble _double)

(define _jsize _jint)

(define-cpointer-type _jobject)
(define-cpointer-type _jclass _jobject)
(define-cpointer-type _jthrowable _jobject)
(define-cpointer-type _jstring _jobject)
(define-cpointer-type _jarray _jobject)
(define-cpointer-type _jbooleanArray _jarray)
(define-cpointer-type _jbyteArray _jarray)
(define-cpointer-type _jcharArray _jarray)
(define-cpointer-type _jshortArray _jarray)
(define-cpointer-type _jintArray _jarray)
(define-cpointer-type _jlongArray _jarray)
(define-cpointer-type _jfloatArray _jarray)
(define-cpointer-type _jdoubleArray _jarray)
(define-cpointer-type _jobjectArray _jarray)

(define _jweak _jobject)

(define _jvalue (_union _jboolean _jbyte _jchar _jshort _jint _jlong
                        _jfloat _jdouble _jobject))

(define-cpointer-type _jfieldID)
(define-cpointer-type _jmethodID)

(define _jobjectRefType (_enum '(JNIInvalidRefType JNILocalRefType
                                 JNIGlobalRefType JNIWeakGlobalRefType)))

(define-cstruct _JNINativeMethod
  ((name _string)
   (signature _string)
   (fnPtr _fpointer)))

(define-syntax (define-jni-interface stx)
  (syntax-case stx ()
    ((_ _if-name ((method-name type ...) ...))
     (and (identifier? #'_if-name)
          (char=? (string-ref (symbol->string (syntax-e #'_if-name)) 0) #\_))
     (let ((if-name (substring (symbol->string (syntax-e #'_if-name)) 1)))
       (with-syntax ((if-name-vt (format-id #'_if-name "~a-vt" if-name))
                     (_if-name-ptr (format-id #'_if-name "_~a-pointer" if-name))
                     (_if-name_vt (format-id #'_if-name "_~a_vt" if-name))
                     (_if-name_vt-ptr (format-id #'_if-name "_~a_vt-pointer" if-name)))
         (define (want-method? method/type)
           (syntax-case method/type ()
             ((_) #f)
             (_ #t)))
         (define (get-name method/type)
           (with-syntax (((method-name . _) method/type))
             #'method-name))
         (define (make-method method-name)
           (with-syntax* ((method-name method-name)
                          (vt-method-name (format-id #'_if-name "~a_vt-~a" if-name #'method-name)))
             #'(...
                (define-syntax-rule (method-name obj args ...)
                  (let ((vt (cast (if-name-vt obj) _pointer _if-name_vt-ptr)))
                    ((vt-method-name vt) obj args ...))))))
         (define (build-type type)
           (syntax-case type ()
             (() #'_fpointer)
             ((type ...) #'(_fun _if-name-ptr type ...))))
         (with-syntax* (((wanted-method-name ...)
                         (map get-name
                              (filter want-method?
                                      (syntax-e #'((method-name type ...) ...)))))
                       ((wanted-method ...)
                        (map make-method (syntax-e #'(wanted-method-name ...))))
                       ((real-type ...) (map build-type (syntax-e #'((type ...) ...)))))
           #'(begin
               (provide _if-name)
               (define-cstruct _if-name ((vt _pointer)))
               (define-cstruct _if-name_vt ((method-name real-type) ...))
               (provide wanted-method-name ...)
               wanted-method ...)))))))

(define-jni-interface _JNIEnv
  ((reserved0)
   (reserved1)
   (reserved2)

   (reserved3)
   (GetVersion -> _jint)

   (DefineClass)
   (FindClass _string -> _jclass)

   (FromReflectedMethod _jobject -> _jmethodID)
   (FromReflectedField _jobject -> _jfieldID)

   (ToReflectedMethod _jclass _jmethodID _jboolean -> _jobject)

   (GetSuperclass _jclass -> _jclass)
   (IsAssignableFrom _jclass _jclass -> _jboolean)

   (ToReflectedField _jclass _jfieldID _jboolean -> _jobject)

   (Throw _jthrowable -> _jint)
   (ThrowNew _jclass _string -> _jint)
   (ExceptionOccurred -> _jthrowable)
   (ExceptionDescribe -> _void)
   (ExceptionClear -> _void)
   (FatalError _string -> _void)

   (PushLocalFrame _jint -> _jint)
   (PopLocalFrame _jobject -> _jobject)

   (NewGlobalRef _jobject -> _jobject)
   (DeleteGlobalRef _jobject -> _void)
   (DeleteLocalRef _jobject -> _void)
   (IsSameObject _jobject _jobject -> _jboolean)
   (NewLocalRef _jobject -> _jobject)
   (EnsureLocalCapacity _jint -> _jint)

   (AllocObject _jclass -> _jobject)
   ;; Must figure out how to do varargs, va_list, etc.
   (NewObject)
   (NewObjectV)
   (NewObjectA _jclass _jmethodID (_list i _jvalue) -> _jobject)

   (GetObjectClass _jobject -> _jclass)
   (IsInstanceOf _jobject _jclass -> _jboolean)

   (GetMethodID _jclass _string _string -> _jmethodID)

   (CallObjectMethod)
   (CallObjectMethodV)
   (CallObjectMethodA _jobject _jmethodID (_list i _jvalue) -> _jobject)

   (CallBooleanMethod)
   (CallBooleanMethodV)
   (CallBooleanMethodA _jobject _jmethodID (_list i _jvalue) -> _jboolean)

   (CallByteMethod)
   (CallByteMethodV)
   (CallByteMethodA _jobject _jmethodID (_list i _jvalue) -> _jbyte)

   (CallCharMethod)
   (CallCharMethodV)
   (CallCharMethodA _jobject _jmethodID (_list i _jvalue) -> _jchar)

   (CallShortMethod)
   (CallShortMethodV)
   (CallShortMethodA _jobject _jmethodID (_list i _jvalue) -> _jshort)

   (CallIntMethod)
   (CallIntMethodV)
   (CallIntMethodA _jobject _jmethodID (_list i _jvalue) -> _jint)

   (CallLongMethod)
   (CallLongMethodV)
   (CallLongMethodA _jobject _jmethodID (_list i _jvalue) -> _jlong)

   (CallFloatMethod)
   (CallFloatMethodV)
   (CallFloatMethodA _jobject _jmethodID (_list i _jvalue) -> _jfloat)

   (CallDoubleMethod)
   (CallDoubleMethodV)
   (CallDoubleMethodA _jobject _jmethodID (_list i _jvalue) -> _jdouble)

   (CallVoidMethod)
   (CallVoidMethodV)
   (CallVoidMethodA _jobject _jmethodID (_list i _jvalue) -> _void)

   (CallNonvirtualObjectMethod)
   (CallNonvirtualObjectMethodV)
   (CallNonvirtualObjectMethodA _jobject _jmethodID (_list i _jvalue) -> _jobject)

   (CallNonvirtualBooleanMethod)
   (CallNonvirtualBooleanMethodV)
   (CallNonvirtualBooleanMethodA _jobject _jmethodID (_list i _jvalue) -> _jboolean)

   (CallNonvirtualByteMethod)
   (CallNonvirtualByteMethodV)
   (CallNonvirtualByteMethodA _jobject _jmethodID (_list i _jvalue) -> _jbyte)

   (CallNonvirtualCharMethod)
   (CallNonvirtualCharMethodV)
   (CallNonvirtualCharMethodA _jobject _jmethodID (_list i _jvalue) -> _jchar)

   (CallNonvirtualShortMethod)
   (CallNonvirtualShortMethodV)
   (CallNonvirtualShortMethodA _jobject _jmethodID (_list i _jvalue) -> _jshort)

   (CallNonvirtualIntMethod)
   (CallNonvirtualIntMethodV)
   (CallNonvirtualIntMethodA _jobject _jmethodID (_list i _jvalue) -> _jint)

   (CallNonvirtualLongMethod)
   (CallNonvirtualLongMethodV)
   (CallNonvirtualLongMethodA _jobject _jmethodID (_list i _jvalue) -> _jlong)

   (CallNonvirtualFloatMethod)
   (CallNonvirtualFloatMethodV)
   (CallNonvirtualFloatMethodA _jobject _jmethodID (_list i _jvalue) -> _jfloat)

   (CallNonvirtualDoubleMethod)
   (CallNonvirtualDoubleMethodV)
   (CallNonvirtualDoubleMethodA _jobject _jmethodID (_list i _jvalue) -> _jdouble)

   (CallNonvirtualVoidMethod)
   (CallNonvirtualVoidMethodV)
   (CallNonvirtualVoidMethodA _jobject _jmethodID (_list i _jvalue) -> _void)

   (GetFieldID _jclass _string _string -> _jfieldID)

   (GetObjectField _jobject _jfieldID -> _jobject)
   (GetBooleanField _jobject _jfieldID -> _jboolean)
   (GetByteField _jobject _jfieldID -> _jbyte)
   (GetCharField _jobject _jfieldID -> _jchar)
   (GetShortField _jobject _jfieldID -> _jshort)
   (GetIntField _jobject _jfieldID -> _jint)
   (GetLongField _jobject _jfieldID -> _jlong)
   (GetFloatField _jobject _jfieldID -> _jfloat)
   (GetDoubleField _jobject _jfieldID -> _jdouble)

   (SetObjectField _jobject _jfieldID _jobject -> _void)
   (SetBooleanField _jobject _jfieldID _jboolean -> _void)
   (SetByteField _jobject _jfieldID _jbyte -> _void)
   (SetCharField _jobject _jfieldID _jchar -> _void)
   (SetShortField _jobject _jfieldID _jshort -> _void)
   (SetIntField _jobject _jfieldID _jint -> _void)
   (SetLongField _jobject _jfieldID _jlong -> _void)
   (SetFloatField _jobject _jfieldID _jfloat -> _void)
   (SetDoubleField _jobject _jfieldID _jdouble -> _void)

   (GetStaticMethodID _jclass _string _string -> _jmethodID)

   (CallStaticObjectMethod)
   (CallStaticObjectMethodV)
   (CallStaticObjectMethodA _jclass _jmethodID (_list i _jvalue) -> _jobject)

   (CallStaticBooleanMethod)
   (CallStaticBooleanMethodV)
   (CallStaticBooleanMethodA _jclass _jmethodID (_list i _jvalue) -> _jboolean)

   (CallStaticByteMethod)
   (CallStaticByteMethodV)
   (CallStaticByteMethodA _jclass _jmethodID (_list i _jvalue) -> _jbyte)

   (CallStaticCharMethod)
   (CallStaticCharMethodV)
   (CallStaticCharMethodA _jclass _jmethodID (_list i _jvalue) -> _jchar)

   (CallStaticShortMethod)
   (CallStaticShortMethodV)
   (CallStaticShortMethodA _jclass _jmethodID (_list i _jvalue) -> _jshort)

   (CallStaticIntMethod)
   (CallStaticIntMethodV)
   (CallStaticIntMethodA _jclass _jmethodID (_list i _jvalue) -> _jint)

   (CallStaticLongMethod)
   (CallStaticLongMethodV)
   (CallStaticLongMethodA _jclass _jmethodID (_list i _jvalue) -> _jlong)

   (CallStaticFloatMethod)
   (CallStaticFloatMethodV)
   (CallStaticFloatMethodA _jclass _jmethodID (_list i _jvalue) -> _jfloat)

   (CallStaticDoubleMethod)
   (CallStaticDoubleMethodV)
   (CallStaticDoubleMethodA _jclass _jmethodID (_list i _jvalue) -> _jdouble)

   (CallStaticVoidMethod)
   (CallStaticVoidMethodV)
   (CallStaticVoidMethodA _jclass _jmethodID (_list i _jvalue) -> _void)

   (GetStaticFieldID _jclass _string _string -> _jfieldID)

   (GetStaticObjectField _jclass _jfieldID -> _jobject)
   (GetStaticBooleanField _jclass _jfieldID -> _jboolean)
   (GetStaticByteField _jclass _jfieldID -> _jbyte)
   (GetStaticCharField _jclass _jfieldID -> _jchar)
   (GetStaticShortField _jclass _jfieldID -> _jshort)
   (GetStaticIntField _jclass _jfieldID -> _jint)
   (GetStaticLongField _jclass _jfieldID -> _jlong)
   (GetStaticFloatField _jclass _jfieldID -> _jfloat)
   (GetStaticDoubleField _jclass _jfieldID -> _jdouble)

   (SetStaticObjectField _jclass _jfieldID _jobject -> _void)
   (SetStaticBooleanField _jclass _jfieldID _jboolean -> _void)
   (SetStaticByteField _jclass _jfieldID _jbyte -> _void)
   (SetStaticCharField _jclass _jfieldID _jchar -> _void)
   (SetStaticShortField _jclass _jfieldID _jshort -> _void)
   (SetStaticIntField _jclass _jfieldID _jint -> _void)
   (SetStaticLongField _jclass _jfieldID _jlong -> _void)
   (SetStaticFloatField _jclass _jfieldID _jfloat -> _void)
   (SetStaticDoubleField _jclass _jfieldID _jdouble -> _void)

   (NewString _string/utf-16 _jsize -> _jstring)
   (GetStringLength _jstring -> _jsize)
   (GetStringChars _jstring (isCopy : (_ptr o _jboolean)) -> (chars : _pointer)
                   -> (values chars isCopy))
   (ReleaseStringChars _jstring _pointer -> _void)

   (NewStringUTF _string/utf-8 -> _jstring)
   (GetStringUTFLength _jstring -> _jsize)
   (GetStringUTFChars _jstring (isCopy : (_ptr o _jboolean)) -> (chars : _pointer)
                      -> (values chars isCopy))
   (ReleaseStringUTFChars _jstring _pointer -> _void)

   (GetArrayLength _jarray -> _jsize)

   (NewObjectArray _jsize _jclass _jobject/null -> _jobjectArray)
   (GetObjectArrayElement _jobjectArray _jsize -> _jobject/null)
   (SetObjectArrayElement _jobjectArray _jsize _jobject/null -> _void)

   (NewBooleanArray _jsize -> _jbooleanArray)
   (NewByteArray _jsize -> _jbyteArray)
   (NewCharArray _jsize -> _jcharArray)
   (NewShortArray _jsize -> _jshortArray)
   (NewIntArray _jsize -> _jintArray)
   (NewLongArray _jsize -> _jlongArray)
   (NewFloatArray _jsize -> _jfloatArray)
   (NewDoubleArray _jsize -> _jdoubleArray)

   (GetBooleanArrayElements _jbooleanArray (isCopy : (_ptr o _jboolean))
                            -> (elems : _pointer) -> (values elems isCopy))
   (GetByteArrayElements _jbyteArray (isCopy : (_ptr o _jboolean))
                         -> (elems : _pointer) -> (values elems isCopy))
   (GetCharArrayElements _jcharArray (isCopy : (_ptr o _jboolean))
                         -> (elems : _pointer) -> (values elems isCopy))
   (GetShortArrayElements _jshortArray (isCopy : (_ptr o _jboolean))
                          -> (elems : _pointer) -> (values elems isCopy))
   (GetIntArrayElements _jintArray (isCopy : (_ptr o _jboolean))
                        -> (elems : _pointer) -> (values elems isCopy))
   (GetLongArrayElements _jlongArray (isCopy : (_ptr o _jboolean))
                         -> (elems : _pointer) -> (values elems isCopy))
   (GetFloatArrayElements _jfloatArray (isCopy : (_ptr o _jboolean))
                          -> (elems : _pointer) -> (values elems isCopy))
   (GetDoubleArrayElements _jdoubleArray (isCopy : (_ptr o _jboolean))
                           -> (elems : _pointer) -> (values elems isCopy))

   (ReleaseBooleanArrayElements _jbooleanArray _pointer _jint -> _void)
   (ReleaseByteArrayElements _jbyteArray _pointer _jint -> _void)
   (ReleaseCharArrayElements _jcharArray _pointer _jint -> _void)
   (ReleaseShortArrayElements _jshortArray _pointer _jint -> _void)
   (ReleaseIntArrayElements _jintArray _pointer _jint -> _void)
   (ReleaseLongArrayElements _jlongArray _pointer _jint -> _void)
   (ReleaseFloatArrayElements _jfloatArray _pointer _jint -> _void)
   (ReleaseDoubleArrayElements _jdoubleArray _pointer _jint -> _void)

   (GetBooleanArrayRegion _jbooleanArray _jsize _jsize _pointer -> _void)
   (GetByteArrayRegion _jbyteArray _jsize _jsize _pointer -> _void)
   (GetCharArrayRegion _jcharArray _jsize _jsize _pointer -> _void)
   (GetShortArrayRegion _jshortArray _jsize _jsize _pointer -> _void)
   (GetIntArrayRegion _jintArray _jsize _jsize _pointer -> _void)
   (GetLongArrayRegion _jlongArray _jsize _jsize _pointer -> _void)
   (GetFloatArrayRegion _jfloatArray _jsize _jsize _pointer -> _void)
   (GetDoubleArrayRegion _jdoubleArray _jsize _jsize _pointer -> _void)

   (SetBooleanArrayRegion _jbooleanArray _jsize _jsize _pointer -> _void)
   (SetByteArrayRegion _jbyteArray _jsize _jsize _pointer -> _void)
   (SetCharArrayRegion _jcharArray _jsize _jsize _pointer -> _void)
   (SetShortArrayRegion _jshortArray _jsize _jsize _pointer -> _void)
   (SetIntArrayRegion _jintArray _jsize _jsize _pointer -> _void)
   (SetLongArrayRegion _jlongArray _jsize _jsize _pointer -> _void)
   (SetFloatArrayRegion _jfloatArray _jsize _jsize _pointer -> _void)
   (SetDoubleArrayRegion _jdoubleArray _jsize _jsize _pointer -> _void)

   (RegisterNatives _jclass _JNINativeMethod-pointer _jint -> _jint)
   (UnregisterNatives _jclass -> _jint)

   (MonitorEnter _jobject -> _jint)
   (MonitorExit _jobject -> _jint)

   (GetJavaVM #;((vm : (_ptr o _JavaVM-pointer)) -> _jint -> vm))
   
   (GetStringRegion _jstring _jsize _jsize _pointer -> _void)
   (GetStringUTFRegion _jstring _jsize _jsize _pointer -> _void)

   (GetPrimitiveArrayCritical _jarray (isCopy : (_ptr o _jboolean))
                              -> (array : _pointer) -> (values array isCopy))
   (ReleasePrimitiveArrayCritical _jarray _pointer _jint -> _void)

   (GetStringCritical _jstring (isCopy : (_ptr o _jboolean)) -> (cstring : _pointer)
                      -> (values cstring isCopy))
   (ReleaseStringCritical _jstring _pointer -> _void)

   (NewWeakGlobalRef _jobject -> _jweak)
   (DeleteWeakGlobalRef _jweak -> _void)

   (ExceptionCheck -> _jboolean)

   (NewDirectByteBuffer _pointer _jlong -> _jobject)
   (GetDirectByteBuffer _jobject -> _pointer)
   (GetDirectBufferCapacity _jobject -> _jlong)

   (GetObjectRefType _jobject -> _jobjectRefType)))

(define-cstruct _JavaVMOption
  ((optionString _string)
   (extraInfo _pointer)))

(define-cstruct _JavaVMInitArgs
  ((version _jint)
   (nOptions _jint)
   (options _JavaVMOption-pointer/null)  ;; XXX figure out how to use _list
   (ignoreUnrecognized _jboolean)))

(define-cstruct _JavaVMAttachArgs
  ((version _jint)
   (name _string)
   (group _jobject)))

(define-jni-interface _JavaVM
  ((reserved0)
   (reserved1)
   (reserved2)

   (DestroyJavaVM -> _jint)
   (AttachCurrentThread (env : (_ptr o _JNIEnv)) (_ptr i _JavaVMAttachArgs)
                        -> _jint -> env)
   (DetachCurrentThread -> _jint)
   (GetEnv (env : (_ptr o _JNIEnv)) _jint -> _jint -> env)
   (AttachCurrentThreadAsDaemon (env : (_ptr o _JNIEnv)) (_ptr i _JavaVMAttachArgs)
                        -> _jint -> env)))

(define (get-registry-java-home subkey)
  (define key (format "SOFTWARE\\JavaSoft\\~a" subkey))
  (define version (get-resource "HKEY_LOCAL_MACHINE"
                                (format "~a\\CurrentVersion" key)))
  (and version (get-resource "HKEY_LOCAL_MACHINE"
                             (format "~a\\~a\\JavaHome" key version))))

(define (get-java-home)
  (cond ((getenv "JAVA_HOME"))
        ((get-registry-java-home "Java Runtime Environment"))
        ((get-registry-java-home "Java Development Kit"))
        ((find-executable-path "java" ".." #t)
         => (lambda (path)
              (simplify-path path #f)))
        (else #f)))

(define (get-jre-subdir name)
  (cond ((get-java-home)
         => (lambda (java-home)
              (ormap (lambda (x)
                       (define subdir (build-path java-home x))
                       (and (directory-exists? subdir) subdir))
                     (list (build-path "jre" name) name))))
        (else #f)))

(define (get-jvm-lib-dirs)
  (cond ((get-jre-subdir "lib")
         => (lambda (jre-lib)
              (map (cut build-path jre-lib <>)
                   '("amd64/server" "i586/server" "i586/client"))))
        (else #f)))

(define (get-jvm-bin-dirs)
  (cond ((get-jre-subdir "bin")
         => (lambda (jre-bin)
              (map (cut build-path jre-bin <>)
                   '("server" "client"))))
        (else #f)))

(define libjvm
  (case (system-type)
    ((unix) (ffi-lib "libjvm" #:get-lib-dirs get-jvm-lib-dirs))
    ((windows) (ffi-lib "jvm" #:get-lib-dirs get-jvm-bin-dirs))))

(define-syntax (define-jni stx)
  (syntax-case stx ()
    ((_ name type ...)
     (with-syntax ((jni-name (format-id #'name "JNI_~a" #'name)))
       #'(begin
           (provide name)
           (define name (get-ffi-obj 'jni-name libjvm (_fun type ...))))))))

(define-jni GetDefaultJavaVMInitArgs (_ptr io _JavaVMInitArgs) -> _jint)
(define-jni CreateJavaVM (vm : (_ptr o _JavaVM-pointer)) (env : (_ptr o _JNIEnv-pointer)) (_ptr i _JavaVMInitArgs)
                         -> _jint -> (values vm env))

(provide make-init-args)
(define (make-init-args version . options)
  (make-JavaVMInitArgs version
                       (length options)
                       (list->cblock (map (cut make-JavaVMOption <> #f) options) _JavaVMOption)
                       #f))
