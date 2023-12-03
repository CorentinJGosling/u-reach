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
