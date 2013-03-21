# -*- coding: utf-8 -*-
<!DOCTYLE html>
<html>
  <head>
    <title>${pagetitle}</title>

% if error is UNDEFINED:
    <meta http-equiv="refresh" content="2;URL='/'">
% endif

    <link rel="stylesheet" href="/static/style.css" type="text/css">
  </head>

% if error is UNDEFINED:
  <body>
% else:
  <body class="error">
% endif

    <div class="information">
      <header>
        <h1>${pagetitle}</h1>
      </header>

      <p>${message}</p>
      <p><a href="/">Click here to continue.</a></p>

      <footer>
        <a href="https://github.com/barrucadu/bookdb">Github</a>
      </footer>
    </div>
  </body>
</html>
