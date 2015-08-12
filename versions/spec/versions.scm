(use versions)



(describe "Create versions"
   (context "string->version"
       (context "Full version"     
                (before each:
                        (set! ($ 'version) (string->version "label1.2.3.4extra")))
                (it "recognizes the major version"
                    (expect (version:major ($ 'version)) (be 1)))
                (it "recognizes the minor version"
                    (expect (version:minor ($ 'version)) (be 2)))
                (it "recognizes the micro version"
                    (expect (version:micro ($ 'version)) (be 3)))
                (it "recognizes the patch version"
                    (expect (version:patch ($ 'version)) (be (list 4))))
                (it "recognizes the extra version"
                    (expect (version:extra ($ 'version)) (be "extra"))))
       
       (context "Partial versions"
                (let ((lmfff (string->version "label1.0"))
                      (fmmff (string->version "1.2")))
                (it "recognizes label major and minor"
                    (expect (version:label lmfff) (be "label"))
                    (expect (version:major lmfff) (be 1))
                    (expect (version:minor lmfff) (be 0)))

                (it "recognizes major and minor"
                    (expect (version:label fmmff) (be false))
                    (expect (version:major fmmff) (be 1))
                    (expect (version:minor fmmff) (be 2))
                    (expect (version:micro fmmff) (be false))
                    (expect (version:patch fmmff) (be false)))))

       (context "Invalid versions"
                (it "doesn't recognize label only"
                    (expect (string->version "label") (be false)))
                (it "doesn't recognize major only"
                    (expect (string->version "1") (be false)))))
   
   (context "version->string"
            (let ((version (make-version 1 0 label: "test" micro: 1 patch: (list 1 2) extra: "foo")))
              (it "convertes correctly"
                  (expect (version->string version) (be "test1.0.1.1.2foo"))))))


(describe "Compare versions"
   (context "version=?"
      (it "recognize equal versions if they're exactly the same"
          (let ((v1 (string->version "1.0.1"))
                (v2 (string->version "1.0.1.1")))

            (expect (version=? v1 v1) (be true))
            (expect (version=? v1 v2)  (be false)))))
   
   (context "version<?"
     (it "knows how to compare major versions"
         (let ((v1 (string->version "1.1.2"))
               (v2 (string->version "2.0.0")))
           (expect (version<? v1 v2) (be true))))
     
     (it "knows how to compare minor versions"
         (let ((v1 (string->version "1.1.2"))
               (v2 (string->version "1.3.0")))
           (expect (version<? v1 v2) (be true))))
     
     (it "knows how to compare micro versions"
         (let ((v1 (string->version "1.1.2.3"))
               (v2 (string->version "1.1.2.4")))
           (expect (version<? v1 v2) (be true))))))

(describe "Sort versions")



(describe "Bump versions"
  (before each:
    (set! ($ 'version) (string->version "1.1.1.1")))


  (context "Functional"

    (it "can bump major"
      (let ((v ($ 'version)))
        (expect (version:major (bump:major v)) (be 2))))

    (it "can bump major to specific version"
      (let ((v ($ 'version)))
        (expect (version:major (bump:major v to: 4)) (be 4))))

    (it "can bump minor"
      (let ((v ($ 'version)))
        (expect (version:minor (bump:minor v)) (be 2))))

    (it "can bump minor to specific version"
      (let ((v ($ 'version)))
        (expect (version:minor (bump:minor v to: 4)) (be 4))))
    
    (it "can bump micro"
      (let ((v ($ 'version)))
        (expect (version:micro (bump:micro v)) (be 2))))

    (it "can bump micro to specific version"
      (let ((v ($ 'version)))
        (expect (version:micro (bump:micro v to: 4)) (be 4))))

    (it "bumps micro if it wasn't set"
      (let ((v (string->version "1.1")))
        (expect (version:micro (bump:micro v)) (be 1))))


    (it "bumps patch"
      (let ((v (string->version "1.2.3.4.5")))
        (expect (version:patch (bump:patch v)) (be (list 4 6)))))
    (it "bumps patch if not set"
      (let ((v (string->version "1.2.3")))
        (expect (version:patch (bump:patch v)) (be (list 1)))))

    (it "bumps patch to specfic version"
      (let ((v (string->version "1.2.3")))
        (expect (version:patch (bump:patch v to: (list 1 2))) (be (list 1 2)))))


    (it "bumps patch if present"
      (let ((v (string->version "1.2.3.1")))
        (expect (version->string (bump v)) (be "1.2.3.2"))))

    (it "bumps micro if present"
      (let ((v (string->version "1.2.3")))
        (expect (version->string (bump v)) (be "1.2.4"))))

    (it "bumps minor if present"
      (let ((v (string->version "1.2")))
        (expect (version->string (bump v)) (be "1.3"))))

    )
  
  (context "Side-effecting"
    
    (it "can bump major"
      (let ((v ($ 'version)))
        (expect (version:major (bump:major! v)) (be 2))))

    (it "can bump major to specific version"
      (let ((v ($ 'version)))
        (expect (version:major (bump:major! v to: 4)) (be 4))))

    (it "can bump minor"
      (let ((v ($ 'version)))
        (expect (version:minor (bump:minor! v)) (be 2))))

    (it "can bump minor to specific version"
      (let ((v ($ 'version)))
        (expect (version:minor (bump:minor! v to: 4)) (be 4))))
    
    (it "can bump micro"
      (let ((v ($ 'version)))
        (expect (version:micro (bump:micro! v)) (be 2))))

    (it "can bump micro to specific version"
      (let ((v ($ 'version)))
        (expect (version:micro (bump:micro! v to: 4)) (be 4))))

    (it "bumps micro if it wasn't set"
      (let ((v (string->version "1.1")))
        (expect (version:micro (bump:micro! v)) (be 1))))


    (it "bumps patch"
      (let ((v (string->version "1.2.3.4.5")))
        (expect (version:patch (bump:patch! v)) (be (list 4 6)))))
    (it "bumps patch if not set"
      (let ((v (string->version "1.2.3")))
        (expect (version:patch (bump:patch! v)) (be (list 1)))))

    (it "bumps patch to specfic version"
      (let ((v (string->version "1.2.3")))
        (expect (version:patch (bump:patch! v to: (list 1 2))) (be (list 1 2)))))


    (it "bumps patch if present"
      (let ((v (string->version "1.2.3.1")))
        (expect (version->string (bump! v)) (be "1.2.3.2"))))

    (it "bumps micro if present"
      (let ((v (string->version "1.2.3")))
        (expect (version->string (bump! v)) (be "1.2.4"))))

    (it "bumps minor if present"
      (let ((v (string->version "1.2")))
        (expect (version->string (bump! v)) (be "1.3")))))


  )

