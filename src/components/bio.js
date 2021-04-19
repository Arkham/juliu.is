/**
 * Bio component that queries for data
 * with Gatsby's StaticQuery component
 *
 * See: https://www.gatsbyjs.org/docs/static-query/
 */

import React from "react"
import { StaticQuery, graphql } from "gatsby"
import { StaticImage } from "gatsby-plugin-image"

import { rhythm } from "../utils/typography"

function Bio() {
  return (
    <StaticQuery
      query={bioQuery}
      render={data => {
        const { author, social } = data.site.siteMetadata
        return (
          <div
            style={{
              display: `flex`,
              marginBottom: rhythm(1),
            }}
          >
            <StaticImage
              layout="fixed"
              formats={["AUTO", "WEBP"]}
              src="../../content/assets/profile-pic.jpg"
              width={50}
              height={50}
              quality={95}
              alt={author}
              style={{
                marginRight: rhythm(1 / 2),
                marginBottom: 0,
                minWidth: 50,
                borderRadius: `100%`,
              }}
              imgStyle={{
                borderRadius: `50%`,
              }}
            />
            <div>
              <p
                style={{
                  marginBottom: 0,
                }}
              >
                Personal blog by{" "}
                <a href={`https://twitter.com/${social.twitter}`}>Ju Liu</a>.
              </p>
              <p
                style={{
                  marginBottom: 0,
                }}
              >{`I try to write code that doesn't suck. I rarely succeed.`}</p>
            </div>
          </div>
        )
      }}
    />
  )
}

const bioQuery = graphql`
  query BioQuery {
    site {
      siteMetadata {
        author
        social {
          twitter
        }
      }
    }
  }
`

export default Bio
