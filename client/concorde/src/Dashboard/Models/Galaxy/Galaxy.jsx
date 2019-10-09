import React from 'react'
import * as THREE from "three";
import DashboardItem from '../../DashboardItem';
import { userService } from '../../../_services';
import { isTryStatement } from '@babel/types';
import { TetrahedronGeometry } from 'three';

class GalaxyCanvas extends React.Component {

  constructor(props) {
    super(props);
    this.state = {
      cameraXYZ: { x: 0, y: 0, z: 25 },
      scene: null,
    }
    this.mountRef = React.createRef();
  }

  componentDidMount() {

    // === THREE.JS CODE START ===
    var scene = new THREE.Scene();
    this.props.onSceneCreated(scene);
    this.setState((state) => {
      return {
        ...state,
        scene: scene,
      }
    });

    var camera = new THREE.PerspectiveCamera( 75, window.innerWidth/window.innerHeight, 0.1, 1000 );
    var renderer = new THREE.WebGLRenderer();
    renderer.setSize( window.innerWidth, window.innerHeight );
    this.mountRef.current.appendChild( renderer.domElement );
    camera.position.x = this.state.cameraXYZ.x;
    camera.position.y = this.state.cameraXYZ.y;
    camera.position.z = this.state.cameraXYZ.z;

    var animate = function () {
      requestAnimationFrame( animate );
      renderer.render( scene, camera );
    };
    animate();
    // === THREE.JS EXAMPLE CODE END ===
  }


  render() {
    return (
      <div ref={this.mountRef}></div>
    )
  }
}

class Galaxy extends React.Component {

  constructor(props) {
    super(props);

    this.onConnected = this.onConnected.bind(this);
    this.loadScene = this.loadScene.bind(this);
    this.getData = this.getData.bind(this);
    this.setScene = this.setScene.bind(this);
}

  onConnected(clientId) {
    this.getData(this.state.scene, clientId);
  }

  getData(scene, clientId) {
    userService.postRequest('client/' + clientId, {data: 'get', sort: 0, ascending: true})
    .then((result) => result.json())   
    .then((resp) => {
        this.loadScene(scene, resp.table.data);
        this.setState(state => {
            return {
                ...state,
                clientId: clientId,
                headings: resp.table.headings,
                data: resp.table.data,
            }                    
        });
    });

  }

  loadScene(scene, data) {
    let textureLoader = new THREE.TextureLoader();
    let starTexture = textureLoader.load("textures/galaxy/star.png");

    const material = new THREE.SpriteMaterial( { map: starTexture, color: 0xffffff, fog: true } );

  for (const item of data) {
      const star = new THREE.Sprite(material);
      star.position.set(item.x, item.y, item.z);
      scene.add(star);      
    }

    // var light = new THREE.PointLight(0xFFFFFF);
    // light.position.set(10, 0, 25);
    // scene.add(light);
  }

  setScene(scene) {
    this.setState(state => { return { ...state, scene: scene}});
  }

  render () {
      return (
        <DashboardItem 
          title={this.props.title} 
          model={this.props.model} 
          modelArg={this.props.modelArgs} 
          onConnected={this.onConnected} 
          onDashboardCommand={this.props.onDashboardCommand}
        >
          <GalaxyCanvas
            onSceneCreated={this.setScene}
          >

          </GalaxyCanvas>
        </DashboardItem>
      );
  }

}

export { Galaxy };