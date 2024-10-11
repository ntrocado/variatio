build:
	sbcl --load variatio.asd \
	     --eval '(ql:quickload :variatio)' \
         --eval "(sb-ext:save-lisp-and-die #p\"variatio\" :toplevel #'variatio:main :executable t)"
