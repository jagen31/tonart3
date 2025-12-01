#lang at-exp racket

(provide gen)

(define (gen code)
@string-append{
<html>
  <head>
    <script type="module" defer>
      import { Chuck } from 'https://cdn.jsdelivr.net/npm/webchuck/+esm';

      let theChuck; // global variable

      document.getElementById('action').addEventListener('click', async () => {
        // Initialize default ChucK object
        if (theChuck === undefined) {
          theChuck = await Chuck.init([]);
        }
        // Run ChucK code
        theChuck.runCode(`
            @code
        `);
      });
    </script>
  </head>
  <body>
    <button id="action">Start and Play</button>
  </body>
</html>
}
)

