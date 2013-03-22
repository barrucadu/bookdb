# -*- coding: utf-8 -*-
<!DOCTYLE html>
<html>
  <head>
    <title>${title}</title>

% if error is UNDEFINED:
    <meta http-equiv="refresh" content="2;URL='${webpath}/'">
% endif

    <link rel="stylesheet" href="${webpath}/static/style.css" type="text/css">
  </head>

% if error is UNDEFINED:
  <body>
% else:
  <body class="error">
% endif

    <div class="information">
      <header>
        <h1>${title}</h1>
      </header>

      <p>${message}</p>
      <p><a href="${webpath}/">Click here to continue.</a></p>

      <footer>
        <a href="https://github.com/barrucadu/bookdb">Github</a>
      </footer>
    </div>
  </body>
</html>
