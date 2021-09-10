<!--
*** Based on the Best-README-Template: https://github.com/othneildrew/Best-README-Template
***
*** To avoid retyping too much info. Do a search and replace for the following:
*** repo_name, project_title, project_description
-->



<!-- PROJECT SHIELDS -->
<!-- [![Release][release-shield]][release-url] -->
<!-- [![Last Commit][last-commit-shield]][last-commit-url] -->
<!-- [![Contributors][contributors-shield]][contributors-url] -->
<!-- [![Forks][forks-shield]][forks-url] -->
<!-- [![Stargazers][stars-shield]][stars-url] -->
<!-- [![Issues][issues-shield]][issues-url] -->
<!-- [![MIT License][license-shield]][license-url] -->
<!-- [![LinkedIn][linkedin-shield]][linkedin-url] -->



<!-- PROJECT LOGO -->
<br />
<p align="center">
<!--   <a href="https://github.com/Tim-W-James/repo_name">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a> -->

  <h2 align="center">Sierpinski Triangle</h2>

  <p align="center">
    Draws a Sierpinski Triangle using a recursive algorithm.
    <br />
    This project was created during my university studies at <b>ANU</b> in <b>2019</b> and has been transferred from the ANU GitLab server.
    <br />
    <a href="https://github.com/Tim-W-James/Sierpinski-Triangle/blob/master/report.pdf"><strong>Read the technical report »</strong></a>
    <br />
    <br />
<!--     <a href="https://github.com/Tim-W-James/repo_name">View Demo</a> -->
<!--     ·
    <a href="https://github.com/Tim-W-James/repo_name/issues">Report Bug</a> -->
<!--     ·
    <a href="https://github.com/Tim-W-James/repo_name/issues">Request Feature</a> -->
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#usage">Usage</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#how-to-use">How to use</a></li>
      </ul>
    </li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgements">Acknowledgements</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

Using the functional programming language [Haskell](https://www.haskell.org/platform/) I created a program that draws a Sierpinski Triangle using a recursive algorithm.
Find the core algorithm in [src/Turtle.hs](https://github.com/Tim-W-James/Sierpinski-Triangle/blob/master/src/Turtle.hs).
To find out how the algorithm has been implemented, read the [report](https://github.com/Tim-W-James/Sierpinski-Triangle/blob/master/report.pdf).

### Built With

* [Haskell Functional Programming Language](https://www.haskell.org/platform/)
* [CodeWorld API](https://hackage.haskell.org/package/codeworld-api/docs/CodeWorld.html)


<!-- USAGE -->
## Usage

### Prerequisites

* To run requires the Global Haskell Compiler and Cabal: https://www.haskell.org/platform/
* To develop use [IntelliJ IDEA](https://www.jetbrains.com/idea/)

### Installation

1. Clone the repo
   ```sh
   git clone https://github.com/Tim-W-James/Sierpinski-Triangle.git
   ```
2. Navigate into the root directory and run
   ```sh
   cabal build
   cabal run
   ```
3. Follow the local host link to a browser


### How to use
| Action                     | Effect                                                                            |
|----------------------------|-----------------------------------------------------------------------------------|
| `T` (key)                  | Draw simple triangle                                                              |
| `P` (key)                  | Draw polygon                                                                      |
| `S` (key)                  | Draw Sierpinski triangle                                                          |
| `C` (key)                  | Draw 'COMP1100' logo                                                              |




<!-- CONTACT -->
## Contact

Email: [tim.jameswork9800@gmail.com](mailto:tim.jameswork9800@gmail.com "tim.jameswork9800@gmail.com")

Project Link: [https://github.com/Tim-W-James/Sierpinski-Triangle](https://github.com/Tim-W-James/Sierpinski-Triangle)



<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements

* Australian National University for project skeleton and CodeWorld API implementation





<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[release-shield]: https://img.shields.io/github/v/release/Tim-W-James/repo_name.svg?include_prereleases&style=for-the-badge
[release-url]: https://github.com/Tim-W-James/repo_name/releases
[last-commit-shield]: https://img.shields.io/github/last-commit/Tim-W-James/repo_name.svg?style=for-the-badge
[last-commit-url]: https://github.com/Tim-W-James/repo_name/commits/main
[contributors-shield]: https://img.shields.io/github/contributors/Tim-W-James/repo_name.svg?style=for-the-badge
[contributors-url]: https://github.com/Tim-W-James/repo_name/graphs/contributors
[contributors-shield]: https://img.shields.io/github/contributors/Tim-W-James/repo_name.svg?style=for-the-badge
[contributors-url]: https://github.com/Tim-W-James/repo_name/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/Tim-W-James/repo_name.svg?style=for-the-badge
[forks-url]: https://github.com/Tim-W-James/repo_name/network/members
[stars-shield]: https://img.shields.io/github/stars/Tim-W-James/repo_name.svg?style=for-the-badge
[stars-url]: https://github.com/Tim-W-James/repo_name/stargazers
[issues-shield]: https://img.shields.io/github/issues/Tim-W-James/repo_name.svg?style=for-the-badge
[issues-url]: https://github.com/Tim-W-James/repo_name/issues
[license-shield]: https://img.shields.io/github/license/Tim-W-James/repo_name?style=for-the-badge
[license-url]: https://github.com/Tim-W-James/repo_name/blob/main/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/timothy-william-james/
[product-screenshot]: screenshot.png

<!-- USEFUL LINKS FOR MARKDOWN
* https://www.markdownguide.org/basic-syntax
* https://www.webpagefx.com/tools/emoji-cheat-sheet
* https://shields.io
* https://choosealicense.com
* https://pages.github.com
* https://daneden.github.io/animate.css
* https://connoratherton.com/loaders
* https://kenwheeler.github.io/slick
* https://github.com/cferdinandi/smooth-scroll
* http://leafo.net/sticky-kit
* http://jvectormap.com
* https://fontawesome.com -->
