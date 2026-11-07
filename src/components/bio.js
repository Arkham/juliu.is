import * as React from "react"
import { StaticImage } from "gatsby-plugin-image"

const Bio = () => {
  return (
    <div className="bio">
      <StaticImage
        className="bio-avatar"
        formats={["auto", "webp", "avif"]}
        src="../images/profile-pic.jpg"
        width={50}
        height={50}
        quality={95}
        alt="Profile picture"
      />
      <div className="bio-text">
        <p>
          A blog by <a href={`https://hachyderm.io/@arkham`}>Ju Liu</a>.
        </p>
        <p>{`I try to write code that doesn't suck. I rarely succeed.`}</p>
      </div>
    </div>
  )
}

export default Bio
