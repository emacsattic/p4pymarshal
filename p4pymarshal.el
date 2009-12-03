;; p4pymarshal.el --- read/write p4 python marshalled objects
;; Author: Noah Friedman <friedman@splode.com>
;; Created: 2006-06-08
;; Public domain.

;; $Id: p4pymarshal.el,v 1.1 2006/09/23 03:45:37 friedman Exp $

;; Commentary:

;; This currently implements only enough of a python object (un)marshaller
;; to interact with the p4 command line client.  It's by no means complete
;; enough to read general python marshalled objects.

;; Code:


;;; unmarshalling functions

(defconst p4py-temp-buffer-name " *p4py work*")

(defconst p4py-load-type-methods
  '((buffer            . p4py-load-stream/buffer)
    (marker            . p4py-load-stream/marker)
    (string            . p4py-load-stream/string)

    (subr              . p4py-load-stream/function)
    (compiled-function . p4py-load-stream/function)
    (cons              . p4py-load-stream/function)
    (symbol            . p4py-load-stream/function)))

(defun p4py-load (stream)
  (let ((load-method (cdr (assq (type-of stream) p4py-load-type-methods))))
    (when load-method
      (funcall load-method stream))))

(defsubst p4py-read-stream/char ()
  (prog1
      (char-after (point))
    (forward-char 1)))

(defsubst p4py-read-stream/string (length)
  (prog1
      (buffer-substring (point) (+ (point) length))
    (forward-char length)))

(defun p4py-load-stream/buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (let ((case-fold-search nil)
          (data nil)
          (keyp 0)
          len key elt)
      (when (char-equal (p4py-read-stream/char) ?{)
        (while (char-equal (p4py-read-stream/char) ?s)
          (setq len (+ (p4py-read-stream/char)
                       (lsh (p4py-read-stream/char)  8)
                       (lsh (p4py-read-stream/char) 16)
                       (lsh (p4py-read-stream/char) 24)))
          (setq data (cons (p4py-read-stream/string len) data))
          (setq keyp (logxor keyp 1)) ; toggle
          (cond (keyp
                 (


            ;; odd

            )



      (nreverse data))))
))))


(defun p4py-load-file (file)
  (let ((buf (generate-new-buffer p4py-temp-buffer-name))
        (data nil)
        elt)
    (set-buffer buf)
    (fundamental-mode)
    (insert-file-contents file nil)
    (goto-char (point-min))
    (while (setq elt (p4py-load buf))
      (setq data (cons elt data)))
    (kill-buffer buf)
    (nreverse data)))


;; Return t if X is a function, `nil' otherwise.
;; X may be a subr, a byte-compiled function, a lambda expression, or a
;; symbol with a function definition.
;; In the last case, no attempt is made to determine if the lambda sexp
;; is actually well-formed (i.e. syntactically valid as a function).
(defun p4py-function-p (x)
  (cond ((subrp x))
        ((and (fboundp 'byte-code-function-p)
              (byte-code-function-p x)))
        ((and (consp x)
              (eq (car x) 'lambda)))
        ((and (symbolp x)
              (fboundp x)))
        (t nil)))


;;
;; sub load
;; {
;;   my ($self, $fh) = @_;
;;
;;   local $_;
;;   return undef unless (_xread ($fh, $_, 2, 0) == 2
;;                        && substr ($_, -2, 1) eq "{");
;;
;;   while (_xread ($fh, $_, 4, length $_) == 4)
;;     {
;;       my $len = unpack ("i", substr ($_, -4, 4));
;;       _xread ($fh, $_, $len + 1, length $_);
;;       last if substr ($_, -1, 1) eq '0';
;;     }
;;   $self->loads ($_);
;; }
;;
;; sub loads
;; {
;;   local $_ = $_[1];  # $_[0] is $self
;;   return undef unless /^{/;
;;
;;   my %dict;
;;   my $key;
;;   my $i = 0;
;;   pos $_ = 1;
;;   while (m/\Gs(....)/cgs)
;;     {
;;       my $len = unpack ("i", $1);
;;       my $tok = substr ($_, pos $_, $len);
;;       pos $_ += $len;
;;
;;       if ($i++ & 1) # odd; key value
;;         {
;;           if ($key =~ /^(.*?)([\d,]*\d)$/)
;;             {
;;               my $key = $1;
;;               my @idx = split (/,/, $2);
;;               my $tail = pop @idx;
;;
;;               $dict{$key} = [] unless exists $dict{$key};
;;               my $node = $dict{$key};
;;               map { $node->[$_] = [] unless defined $node->[$_];
;;                     $node = $node->[$_];
;;                   } @idx;
;;               $node->[$tail] = $tok;
;;             }
;;           else
;;             {
;;               $dict{$key} = $tok
;;             }
;;         }
;;       else { $key = lc $tok } # even; key name
;;     }
;;   return \%dict;
;; }
;;
;; 
;; ;;## marshalling methods
;;
;; sub dumpfile
;; {
;;   my ($self, $list, $file) = @_;
;;
;;   $list = [$list] unless ref $list eq 'ARRAY';
;;
;;   my $fh = ref $file ? $file : gensym;
;;   unless (ref $file)
;;     {
;;       # truncate on open?
;;       my $mode = O_WRONLY|O_CREAT|O_APPEND|O_TRUNC;
;;       sysopen ($fh, $file, $mode, 0666) || return;
;;     }
;;   map { $self->dump ($_, $fh) } @$list;
;;   close ($fh) unless ref $file;
;; }
;;
;; sub dump
;; {
;;   my ($self, $obj, $fh) = @_;
;;   syswrite ($fh, $self->dumps ($obj));
;; }
;;
;; sub dumps
;; {
;;   my @str;
;;   while (my @pair = each %{$_[1]})
;;     {
;;       push @str, (ref $pair[1] eq 'ARRAY'
;;                   ? _dumps_a (@pair)
;;                   : _dumps_s (@pair));
;;     }
;;   join ("", "{", @str, "0");
;; }
;;
;; 
;; ;;## packing routines for various data types
;;
;; sub _dumps_s # not a method; no $self
;; {
;;   join ("", map { ("s", pack ("i", length $_), $_) } @_);
;; }
;;
;; ;; TODO: make this handle nested array references
;; sub _dumps_a # not a method; no $self
;; {
;;   my $i = 0;
;;   _dumps_s (map { (sprintf ("%s%d", $_[0], $i++), $_) } @{$_[1]});
;; }
;;
;; ;;## misc private non-method functions
;;
;; ;; read as much as possible until eof; don't return partial reads
;; sub _xread
;; {
;;   my ($fh, $size, $offset) = @_[0,2,3];  # buffer $_[1] modified in-place
;;
;;   return 0 unless defined $size && $size > 0;
;;   $offset = 0 unless defined $offset;
;;
;;   my $total = 0;
;;   while ($total < $size)
;;     {
;;       my $rsz = sysread ($fh, $_[1], $size - $total, $offset + $total);
;;       return $rsz if $rsz < 0; # error
;;       last if $rsz == 0;       # eof
;;       $total += $rsz;
;;     }
;;   return $total;
;; }
;;
;; 1;
