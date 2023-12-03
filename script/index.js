document.addEventListener("DOMContentLoaded", function () {
  const sections = document.querySelectorAll("section");
  const navLinks = document.querySelectorAll("nav a");

  if (sections.length === navLinks.length) {
    function highlightNavLink() {
      let scrolled = false;

      if (document.documentElement.scrollTop === 0) {
        navLinks.forEach((link) => {
          link.classList.remove("active");
        });
        navLinks[0].classList.add("active");
        scrolled = false;
      } else {
        sections.forEach((section, idx) => {
          const rect = section.getBoundingClientRect();
          const threshold = window.innerHeight * 0.5;
          const isVisible = rect.top <= threshold && rect.bottom >= threshold;

          if (isVisible) {
            navLinks.forEach((link) => {
              link.classList.remove("active");
            });
            navLinks[idx].classList.add("active");
            scrolled = true;
          }
        });
      }

      if (!scrolled) {
        navLinks.forEach((link) => {
          link.classList.remove("active");
        });
        navLinks[0].classList.add("active");
      }
    }

    window.addEventListener("scroll", highlightNavLink);
    window.addEventListener("resize", highlightNavLink);
    highlightNavLink();
  } else {
    console.error(
      "Number of sections doesn't match number of navigation links."
    );
  }
});
//
//
//
const readMoreButtonMS = document.getElementById("readmore_buttonMS");
const readMoreContentMS = document.getElementById("read_more_textMS");
const plusIconMS = document.getElementById("plusIconMS");
const readMoreButtonCG = document.getElementById("readmore_buttonCG");
const readMoreContentCG = document.getElementById("read_more_textCG");
const plusIconCG = document.getElementById("plusIconCG");
const readMoreButtonSC = document.getElementById("readmore_buttonSC");
const readMoreContentSC = document.getElementById("read_more_textSC");
const plusIconSC = document.getElementById("plusIconSC");
// const readMoreButtonJF = document.getElementById("readmore_buttonJF");
// const readMoreContentJF = document.getElementById("read_more_textJF");
// const plusIconJF = document.getElementById("plusIconJF");
const readMoreButtonRD = document.getElementById("readmore_buttonRD");
const readMoreContentRD = document.getElementById("read_more_textRD");
const plusIconRD = document.getElementById("plusIconRD");
const readMoreButtonPFP = document.getElementById("readmore_buttonPFP");
const readMoreContentPFP = document.getElementById("read_more_textPFP");
const plusIconPFP = document.getElementById("plusIconPFP");
// const readMoreButtonEV = document.getElementById("readmore_buttonEV");
// const readMoreContentEV = document.getElementById("read_more_textEV");
// const plusIconEV = document.getElementById("plusIconEV");

readMoreButtonMS.addEventListener("click", function () {
  readMoreButtonMS.classList.toggle("rmv_border");
  readMoreContentMS.classList.toggle("show-read-more");
  readMoreContentMS.classList.toggle("add_border");
  plusIconMS.classList.toggle("rotate-45");
});

readMoreButtonCG.addEventListener("click", function () {
  readMoreButtonCG.classList.toggle("rmv_border");
  readMoreContentCG.classList.toggle("show-read-more");
  readMoreContentCG.classList.toggle("add_border");
  plusIconCG.classList.toggle("rotate-45");
});

readMoreButtonSC.addEventListener("click", function () {
  readMoreButtonSC.classList.toggle("rmv_border");
  readMoreContentSC.classList.toggle("show-read-more");
  readMoreContentSC.classList.toggle("add_border");
  plusIconSC.classList.toggle("rotate-45");
});

// readMoreButtonJF.addEventListener("click", function () {
//   readMoreButtonJF.classList.toggle("rmv_border");
//   readMoreContentJF.classList.toggle("show-read-more");
//   readMoreContentJF.classList.toggle("add_border");
//   plusIconJF.classList.toggle("rotate-45");
// });
readMoreButtonRD.addEventListener("click", function () {
  readMoreButtonRD.classList.toggle("rmv_border");
  readMoreContentRD.classList.toggle("show-read-more");
  readMoreContentRD.classList.toggle("add_border");
  plusIconRD.classList.toggle("rotate-45");
});
// readMoreButtonEV.addEventListener("click", function () {
//   readMoreButtonEV.classList.toggle("rmv_border");
//   readMoreContentEV.classList.toggle("show-read-more");
//   readMoreContentEV.classList.toggle("add_border");
//   plusIconEV.classList.toggle("rotate-45");
// });
readMoreButtonPFP.addEventListener("click", function () {
  readMoreButtonPFP.classList.toggle("rmv_border");
  readMoreContentPFP.classList.toggle("show-read-more");
  readMoreContentPFP.classList.toggle("add_border");
  plusIconPFP.classList.toggle("rotate-45");
});
var NUM_PARTICLES = 300,
  MAX_SPEED = 0.00001,
  particles = [],
  imageData,
  pixels,
  w = window.innerWidth,
  h = window.innerHeight,
  mouse = {
    x: 0,
    y: 0,
  },
  canvasParticles = document.querySelector(".js-canvas-particles"),
  ctxParticles = canvasParticles.getContext("2d");

init();

function init() {
  initEvents();
  initStage();

  run();
}

function initEvents() {
  window.addEventListener("resize", initStage);
  document.addEventListener("mousemove", onMouseMove);
}

function initStage() {
  w = window.innerWidth;
  h = window.innerHeight;

  canvasParticles.setAttribute("width", w);
  canvasParticles.setAttribute("height", h);

  initParticles();
}

function onMouseMove(e) {
  mouse = {
    x: e.clientX,
    y: e.clientY,
  };
}

function initParticles() {
  particles = [];

  var i = NUM_PARTICLES,
    p,
    x,
    y,
    velX,
    velY,
    r;

  while (i--) {
    x = randomBetween(0, w);
    y = randomBetween(0, h);
    r = randomBetween(1, 3);

    velX = randomBetween(-MAX_SPEED, MAX_SPEED);
    velY = randomBetween(-MAX_SPEED, MAX_SPEED);

    p = new Particle(x, y, velX, velY, r);
    particles.push(p);
  }
}

function Particle(x, y, velX, velY, r) {
  this.x = x;
  this.y = y;
  this.velX = velX;
  this.velY = velY;
  this.radius = r;

  this.update = function () {
    this.x += this.velX;
    this.y += this.velY;

    this.x = Math.round(this.x);
    this.y = Math.round(this.y);

    if (this.x <= 0 || this.x >= w) {
      this.velX = -this.velX;
    }

    if (this.y <= 0 || this.y >= h) {
      this.velY = -this.velY;
    }
  };

  this.distanceTo = function (p) {
    var dx = p.x - this.x,
      dy = p.y - this.y;

    return Math.sqrt(dx * dx + dy * dy);
  };

  this.getIndex = function () {
    return ((this.x | 0) + (this.y | 0) * w) * 4;
  };
}

function run() {
  window.requestAnimationFrame(run);

  ctxParticles.clearRect(0, 0, w, h);

  var i = particles.length,
    distance,
    distanceMouse,
    q,
    p1,
    p2;

  while (i--) {
    p1 = particles[i];
    p1.update();

    ctxParticles.beginPath();
    ctxParticles.fillStyle = "rgba(255, 255, 255, 0.8)";
    ctxParticles.arc(p1.x, p1.y, p1.radius, 0, 2 * Math.PI, false);
    ctxParticles.fill();
    ctxParticles.closePath();

    distanceMouse = p1.distanceTo(mouse);

    if (distanceMouse <= w * 0.2) {
      connect(p1, mouse);
    }

    for (q = 0; q < particles.length; q++) {
      p2 = particles[q];
      distance = p2.distanceTo(p1);

      if (p2 !== p1 && distance <= w * 0.05) {
        connect(p1, p2);
      }
    }
  }
}

function connect(p1, p2) {
  ctxParticles.beginPath();
  ctxParticles.strokeStyle = "rgba(255, 255, 255, 0.2)";

  ctxParticles.moveTo(p1.x, p1.y);
  ctxParticles.lineTo(p2.x, p2.y);
  ctxParticles.stroke();
  ctxParticles.closePath();
}

// util functions
function randomBetween(min, max, round) {
  var rand = Math.random() * (max - min + 1) + min;
  if (round === true) {
    return Math.floor(rand);
  } else {
    return rand;
  }
}
