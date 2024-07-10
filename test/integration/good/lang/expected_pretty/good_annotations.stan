/* $ stanc --auto-format ../good_annotations.stan : [exit 125] */
warning: Unknown annotation 'foo' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:2:18
  1 │  functions {
  2 │    @foo @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    │                   ^^^ Unknown annotation 'foo' will be ignored by the compiler.
  3 │      print(x);
warning: Unknown annotation 'biz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:2:18
  1 │  functions {
  2 │    @foo @biz void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f){
    │                   ^^^ Unknown annotation 'biz' will be ignored by the compiler.
  3 │      print(x);
warning: Unknown annotation 'baz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:7:20
  6 │  parameters {
  7 │    @baz matrix[3,3] A;
    │                     ^ Unknown annotation 'baz' will be ignored by the compiler.
  8 │  }
warning: Unknown annotation 'bar' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:12:90
 11 │  generated quantities {
 12 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │                                                                                           ^^^^^^^^^^^^^^ Unknown annotation 'bar' will be ignored by the compiler.
 13 │  }
warning: Unknown annotation 'baz' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:12:90
 11 │  generated quantities {
 12 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │                                                                                           ^^^^^^^^^^^^^^ Unknown annotation 'baz' will be ignored by the compiler.
 13 │  }
warning: Unknown annotation 'flux' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:12:90
 11 │  generated quantities {
 12 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │                                                                                           ^^^^^^^^^^^^^^ Unknown annotation 'flux' will be ignored by the compiler.
 13 │  }
warning: Unknown annotation 'really_extra_long_now' will be ignored by the compiler.
    ┌─ ../good_annotations.stan:12:90
 11 │  generated quantities {
 12 │    @bar @baz @flux /* comment in an odd place */ @really_extra_long_now matrix[34, 10000] a_bit_too_long = rep_matrix(1, 34, 10000);
    │                                                                                           ^^^^^^^^^^^^^^ Unknown annotation 'really_extra_long_now' will be ignored by the compiler.
 13 │  }
Internal compiler error:
Pretty-printed program does not match the original!
((functionblock
  (((stmts
     (((stmt
        (FunDef (returntype Void) (funname ((name bar) (id_loc <opaque>)))
         (arguments
          ((AutoDiffable UInt ((name x) (id_loc <opaque>)))
           (AutoDiffable UInt ((name y) (id_loc <opaque>)))
           (AutoDiffable UInt ((name z) (id_loc <opaque>)))
           (AutoDiffable UInt ((name w) (id_loc <opaque>)))
           (AutoDiffable UInt ((name a) (id_loc <opaque>)))
           (AutoDiffable UInt ((name b) (id_loc <opaque>)))
           (AutoDiffable UInt ((name d) (id_loc <opaque>)))
           (AutoDiffable UInt ((name e) (id_loc <opaque>)))
           (AutoDiffable UInt ((name f) (id_loc <opaque>)))))
         (annotations (foo biz))
         (body
          ((stmt
            (Block
             (((stmt
                (Print
                 ((PExpr
                   ((expr (Variable ((name x) (id_loc <opaque>))))
                    (emeta ((loc <opaque>))))))))
               (smeta ((loc <opaque>)))))))
           (smeta ((loc <opaque>)))))))
       (smeta ((loc <opaque>))))))
    (xloc <opaque>))))
 (datablock ()) (transformeddatablock ())
 (parametersblock
  (((stmts
     (((stmt
        (VarDecl
         (decl_type
          (SMatrix AoS ((expr (IntNumeral 3)) (emeta ((loc <opaque>))))
           ((expr (IntNumeral 3)) (emeta ((loc <opaque>))))))
         (transformation Identity) (is_global true) (annotations (baz))
         (variables
          (((identifier ((name A) (id_loc <opaque>))) (initial_value ()))))))
       (smeta ((loc <opaque>))))))
    (xloc <opaque>))))
 (transformedparametersblock ()) (modelblock ())
 (generatedquantitiesblock
  (((stmts
     (((stmt
        (VarDecl
         (decl_type
          (SMatrix AoS ((expr (IntNumeral 34)) (emeta ((loc <opaque>))))
           ((expr (IntNumeral 10000)) (emeta ((loc <opaque>))))))
         (transformation Identity) (is_global true)
         (annotations (bar baz flux really_extra_long_now))
         (variables
          (((identifier ((name a_bit_too_long) (id_loc <opaque>)))
            (initial_value
             (((expr
                (FunApp () ((name rep_matrix) (id_loc <opaque>))
                 (((expr (IntNumeral 1)) (emeta ((loc <opaque>))))
                  ((expr (IntNumeral 34)) (emeta ((loc <opaque>))))
                  ((expr (IntNumeral 10000)) (emeta ((loc <opaque>)))))))
               (emeta ((loc <opaque>)))))))))))
       (smeta ((loc <opaque>))))))
    (xloc <opaque>))))
 (comments <opaque>))
((functionblock
  (((stmts
     (((stmt
        (FunDef (returntype Void) (funname ((name bar) (id_loc <opaque>)))
         (arguments
          ((AutoDiffable UInt ((name x) (id_loc <opaque>)))
           (AutoDiffable UInt ((name y) (id_loc <opaque>)))
           (AutoDiffable UInt ((name z) (id_loc <opaque>)))
           (AutoDiffable UInt ((name w) (id_loc <opaque>)))
           (AutoDiffable UInt ((name a) (id_loc <opaque>)))
           (AutoDiffable UInt ((name b) (id_loc <opaque>)))
           (AutoDiffable UInt ((name d) (id_loc <opaque>)))
           (AutoDiffable UInt ((name e) (id_loc <opaque>)))
           (AutoDiffable UInt ((name f) (id_loc <opaque>)))))
         (annotations ())
         (body
          ((stmt
            (Block
             (((stmt
                (Print
                 ((PExpr
                   ((expr (Variable ((name x) (id_loc <opaque>))))
                    (emeta ((loc <opaque>))))))))
               (smeta ((loc <opaque>)))))))
           (smeta ((loc <opaque>)))))))
       (smeta ((loc <opaque>))))))
    (xloc <opaque>))))
 (datablock ()) (transformeddatablock ())
 (parametersblock
  (((stmts
     (((stmt
        (VarDecl
         (decl_type
          (SMatrix AoS ((expr (IntNumeral 3)) (emeta ((loc <opaque>))))
           ((expr (IntNumeral 3)) (emeta ((loc <opaque>))))))
         (transformation Identity) (is_global true) (annotations (baz))
         (variables
          (((identifier ((name A) (id_loc <opaque>))) (initial_value ()))))))
       (smeta ((loc <opaque>))))))
    (xloc <opaque>))))
 (transformedparametersblock ()) (modelblock ())
 (generatedquantitiesblock
  (((stmts
     (((stmt
        (VarDecl
         (decl_type
          (SMatrix AoS ((expr (IntNumeral 34)) (emeta ((loc <opaque>))))
           ((expr (IntNumeral 10000)) (emeta ((loc <opaque>))))))
         (transformation Identity) (is_global true)
         (annotations (bar baz flux really_extra_long_now))
         (variables
          (((identifier ((name a_bit_too_long) (id_loc <opaque>)))
            (initial_value
             (((expr
                (FunApp () ((name rep_matrix) (id_loc <opaque>))
                 (((expr (IntNumeral 1)) (emeta ((loc <opaque>))))
                  ((expr (IntNumeral 34)) (emeta ((loc <opaque>))))
                  ((expr (IntNumeral 10000)) (emeta ((loc <opaque>)))))))
               (emeta ((loc <opaque>)))))))))))
       (smeta ((loc <opaque>))))))
    (xloc <opaque>))))
 (comments <opaque>))
Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
Called from Frontend__Pretty_print_prog.pretty_print_program in file "src/frontend/Pretty_print_prog.ml", line 76, characters 2-44
Called from Driver__Entry.stan2mir in file "src/driver/Entry.ml", lines 82-87, characters 9-45
Called from Driver__Entry.stan2cpp in file "src/driver/Entry.ml", lines 153-154, characters 4-24
Called from Dune__exe__Stanc.stanc.(fun) in file "src/stanc/stanc.ml", lines 70-73, characters 4-64
Called from Std__Return.with_return in file "src/std/Return.ml", line 9, characters 14-22
Re-raised at Std__Return.with_return in file "src/std/Return.ml", line 14, characters 38-45
Called from Common__ICE.with_exn_message in file "src/common/ICE.ml", line 15, characters 9-15


This should never happen. Please file a bug at %PKG_ISSUES%
and include this message and the model that caused this issue.
