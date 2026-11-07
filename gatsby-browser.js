// custom typefaces
import "@fontsource-variable/montserrat"
import "@fontsource/merriweather"
import "@fontsource/merriweather/700.css"
import "@fontsource/fira-mono"

// normalize CSS across browsers
import "./src/normalize.css"
// custom CSS styles
import "./src/style.css"

// Highlighting for code blocks
import "prismjs/themes/prism-solarizedlight.css"

/**
 * Handle same-page (hash) navigations ourselves. Gatsby's scroll
 * restoration doesn't reset the scroll position when pressing the
 * browser back button from an in-page anchor link.
 *
 * @type {import('gatsby').GatsbyBrowser['shouldUpdateScroll']}
 */
export const shouldUpdateScroll = ({ routerProps, prevRouterProps }) => {
  const location = routerProps.location
  const prevLocation = prevRouterProps && prevRouterProps.location

  // Only customize hash-only navigations within the same page
  if (!prevLocation || prevLocation.pathname !== location.pathname) {
    return true
  }

  if (location.hash) {
    const target = document.querySelector(location.hash)
    if (target) {
      target.scrollIntoView()
    }
  } else {
    // Back from an anchor link: return to the top of the page
    window.scrollTo(0, 0)
  }

  return false
}
