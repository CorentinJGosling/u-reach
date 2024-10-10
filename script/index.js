document.addEventListener("DOMContentLoaded", function () {
  fetch("/img/background_main_img.svg")
    .then((response) => response.text())
    .then((svgContent) => {
      const svgContainer = document.getElementById("svgContainer");
      svgContainer.innerHTML = svgContent;

      const svgElement = svgContainer.querySelector("svg");
      if (svgElement) {
        const figIds = ["step_one", "step_two", "step_three", "step_four"];
        const tooltips = {
          step_one:
            "All U-REACH projects starts by a comprehensive synthesis of any systematic review with (or without) meta-analysis on a broad topic.",
          step_two:
            "Then, a critical step is to appraise the methodological quality of both the reviews, and the primary studies.",
          step_three:
            "Once all studies have been synthesized, it is time to assess the effects of the predictors, and to rate the certainty of evidence.",
          step_four:
            "Last (but not least), you need to create a web platform to share this info with a wide audience. Do not worry, you will find open code of our platforms below !",
        };

        // Create a single tooltip element
        const tooltip = document.createElement("div");
        tooltip.className = "tooltip";
        document.body.appendChild(tooltip);

        function showTooltip(event, text) {
          tooltip.innerText = text;
          tooltip.style.display = "block";
          positionTooltip(event);
        }

        function hideTooltip() {
          tooltip.style.display = "none";
        }

        function positionTooltip(event) {
          const tooltipRect = tooltip.getBoundingClientRect();
          const scrollLeft =
            window.pageXOffset || document.documentElement.scrollLeft;
          const scrollTop =
            window.pageYOffset || document.documentElement.scrollTop;

          let left = event.pageX + 40; // 40px to the right of the cursor
          let top = event.pageY - tooltipRect.height / 2; // Vertically center with the cursor

          // Adjust if tooltip goes off the right edge of the window
          if (left + tooltipRect.width > window.innerWidth + scrollLeft) {
            left = event.pageX - tooltipRect.width - 10; // 10px to the left of the cursor
          }

          // Adjust if tooltip goes off the bottom edge of the window
          if (top + tooltipRect.height > window.innerHeight + scrollTop) {
            top = window.innerHeight + scrollTop - tooltipRect.height - 10;
          }

          tooltip.style.left = `${left}px`;
          tooltip.style.top = `${top}px`;
        }

        function handleMouseOver(event, figId) {
          const figElement = svgElement.getElementById(figId);
          if (figElement) {
            figElement.classList.add("hovered");
            showTooltip(event, tooltips[figId] || "Tooltip text");
          }
        }

        function handleMouseOut(figId) {
          const figElement = svgElement.getElementById(figId);
          if (figElement) {
            figElement.classList.remove("hovered");
          }
          hideTooltip();
        }

        function handleMouseMove(event) {
          if (tooltip.style.display === "block") {
            positionTooltip(event);
          }
        }

        figIds.forEach((figId) => {
          const figElement = svgElement.getElementById(figId);
          if (figElement) {
            figElement.addEventListener("mouseover", (event) =>
              handleMouseOver(event, figId)
            );
            figElement.addEventListener("mouseout", () =>
              handleMouseOut(figId)
            );
            figElement.addEventListener("mousemove", handleMouseMove);
          }

          const textElement = svgElement.getElementById(
            `text_${figId.split("_")[1]}`
          );
          if (textElement) {
            textElement.addEventListener("mouseover", (event) =>
              handleMouseOver(event, figId)
            );
            textElement.addEventListener("mouseout", () =>
              handleMouseOut(figId)
            );
            textElement.addEventListener("mousemove", handleMouseMove);
          }
        });
      } else {
        console.error("SVG element not found.");
      }
    })
    .catch((error) => console.error("Error loading SVG:", error));
});
