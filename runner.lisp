(push #p"./" asdf:*central-registry*)
(asdf:oos 'asdf:load-op :pass-storage)
(pass-storage:main)
(quit)

