import React from 'react'
import * as THREE from "three";

import { Model3D } from '../Model3D';

class Galaxy extends React.Component {

  constructor(props) {
    super(props);

    this.state = {
      cameraXYZ: { x: 0, y: 0, z: 25 },
      scene: null,
      camera: null,
      raycaster: null,
      intersectedObject: null,
    }

    this.initScene = this.initScene.bind(this);
    this.loadScene = this.loadScene.bind(this);
    this.mouseMove = this.mouseMove.bind(this);
}

initScene(scene, width, height) {
  let camera = new THREE.PerspectiveCamera( 75, width / height, 0.1, 1000 );
  camera.position.x = this.state.cameraXYZ.x;
  camera.position.y = this.state.cameraXYZ.y;
  camera.position.z = this.state.cameraXYZ.z;

  let raycaster = new THREE.Raycaster();

  this.setState({
    scene: scene,
    camera: camera,
    raycaster: raycaster,
    intersectedObject: null,
  });

  return camera;
}

loadScene(data) {
    let textureLoader = new THREE.TextureLoader();
    let starTexture = textureLoader.load("textures/galaxy/star.png");

    for (const item of data) {
        const material = new THREE.SpriteMaterial( { map: starTexture, color: item.color } );
        const star = new THREE.Sprite(material);
        star.position.set(item.x, item.y, item.z);
        star.name = item.name;
        this.state.scene.add(star);      
      }
  }

  mouseMove(mouse) {

    const { scene, camera, raycaster } = this.state;

    let intersectedObject = this.state.intersectedObject;
    
    if (raycaster) {
      raycaster.setFromCamera(mouse, camera);

      var intersects = raycaster.intersectObjects( scene.children );

      if ( intersects.length > 0 )
      {
        if ( intersects[0].object !== intersectedObject ) 
        {
          if ( intersectedObject ) {
            intersectedObject.material.color.setHex( intersectedObject.currentHex );
          }

          intersectedObject = intersects[ 0 ].object;
          intersectedObject.currentHex = intersectedObject.material.color.getHex();
          intersectedObject.material.color.setHex( 0xffff00 );
          
          console.log("intersect", intersectedObject);
        }
      } 
      else
      {
        if ( intersectedObject ) {
          intersectedObject.material.color.setHex( intersectedObject.currentHex );
        }
        intersectedObject = null;
      }

      this.setState({
        intersectedObject: intersectedObject,
      })
   }
  }

  render () {
      return (
        <Model3D
        title={this.props.title} 
        model={this.props.model} 
        modelArg={this.props.modelArgs} 
        onDashboardCommand={this.props.onDashboardCommand}
        initScene={this.initScene}
        loadScene={this.loadScene}
        mouseMove={this.mouseMove}
      >
      </Model3D>
      );
  }

}

export { Galaxy };