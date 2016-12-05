function embed(rootDomNode, label)
{
    'use strict';
    var rAF = typeof requestAnimationFrame !== 'undefined'
        ? requestAnimationFrame
        : function(cb) { setTimeout(cb, 1000 / 60); };

    var geometryDecoder = function (g) {
      var rect = g.currentTarget.getBoundingClientRect();

      var set = function (x, y) {
          return {rect: rect, x: x - rect.left, y: y - rect.top};
        };

      if ((g.clientX == null) || (g.clientY == null)) {
        if(g.touches == null) {
          return null
        } else {
          var touch = g.touches[0]
          return set(touch.touchX, touch.touchY);
        }
      } else {
        if ((g.clientX === 0.0) && (g.clientY === 0.0)) {
          return {rect: rect, x: rect.width / 2.0, y: rect.height / 2.0};
        } else {
          return set(g.clientX, g.clientY);
        }
      }
    };

    var isVisible = false,
        metrics = null;


    var button = document.createElement('button');

    button.className =  "mdl-js-ripple-effect mdl-js-button mdl-button mdl-button--raised"

    var ripple = function () {
      button.blur();
      isVisible = false
      rAF(update);

    }
    button.addEventListener('mouseup', ripple);
    button.addEventListener('mouseleave', ripple);
    button.addEventListener('ontouchend', ripple);

    var buttonHandler = function eventHandler(event)
    {
      isVisible = true
      metrics = geometryDecoder(event)
      rAF(update);
    };

    button.addEventListener('mousedown', buttonHandler);
    button.addEventListener('touchstart', buttonHandler);

    button.appendChild(document.createTextNode(label));

    var span = document.createElement('span');

    button.appendChild(span);

    rootDomNode.appendChild(button);

    var toPx = function (k) {
      return Math.round(k) + 'px';
    };

    function update()
    {
        if (metrics != null) {
          var r = metrics.rect;

          var offset = 'translate(' + toPx(metrics.x) + ', ' + toPx(metrics.y) + ')';
          var rippleSize = toPx(
            (Math.sqrt((r.width * r.width) + (r.height * r.height)) * 2.0) + 2.0);
          var scale = isVisible ? 'scale(0.0001, 0.0001)' : '';
          var transformString = 'translate(-50%, -50%) ' + offset + scale;

          var style = span.style

          style.width = rippleSize
          style.height = rippleSize
          style['-webkit-transform'] = transformString
          style['-ms-transform'] = transformString
          style.transform = transformString

        }

        span.className = 'mdl-ripple'
        if (isVisible) {
          span.className += ' is-visible'
        } else {
          span.className += ' is-animating'
        }

    }

    update()

    return button
}
